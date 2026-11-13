# Engineering Manager (Kingkong)

Kingkong is the OpenClaw agent (`main`) on botserver. It runs the whole `primarykids` Multica
workspace and keeps the conversation with Ethan about that work. Use it as a *source of claims*,
never as the final word on state — git, then Jira, outrank it. It can't see work done outside
Multica, nor GitHub merge/CI state.

## Two layers — don't use the agent for what a command can tell you

**Layer 1: board state — run the commands directly (about half a second each).** They need a
project (`KK_PROJECT`); the ones Ethan works in are `pk-workers-monorepo`, `pk-shopify-theme`
and `rfd`. Save to files, don't print them all:

```bash
for p in pk-workers-monorepo pk-shopify-theme rfd; do
  ssh botserver-openclaw "KK_PROJECT=$p kk-board" > ~/.cache/eca/kk-board-$p.txt 2>&1
done
```

`kk-board` groups tickets by epic with status, owner, age and the Jira key. `kk-graph <epic>`
shows chains and dependencies; `kk-sync` compares board vs Jira (needs `--project` or
`KK_PROJECT`). A non-zero exit or an "error:" first line means that layer failed — say so.

**Layer 2: the agent — only for what lives nowhere else.** Ask it for (1) decisions or
agreements made with Ethan in conversation, (2) what is waiting on Ethan, (3) anywhere what
Ethan told it disagrees with the board, Jira or GitHub. Never ask it to list tickets or
statuses: that is Layer 1, and it is what made the first version take about 5 minutes
(17 tool calls). The narrow question took 97 seconds.

## Asking the agent (detached, output to a file)

Do **not** rely on background-job output: it only shows new text, the final answer gets lost
when the job ends, and the job disappears afterwards (this cost a whole briefing once). Write
to files and read the files:

```bash
cd ~/.cache/eca && rm -f kk-out.json kk-err.txt kk-elapsed.txt && S=$(date +%s) && (
  ssh botserver-openclaw "node /home/openclaw/.local/opt/openclaw/dist/index.js agent --agent main \
    --session-id eca-briefing-$(date +%F) --thinking low --timeout 300 --json \
    --message '<prompt>'" > kk-out.json 2> kk-err.txt
  echo $(( $(date +%s) - S )) > kk-elapsed.txt )
```

Start it (as a background shell job) in the same step as the other gathering. Use a fresh
`--session-id`, no `--deliver` (nothing is sent to chat), and `--thinking low`. The harmless
stderr line `slow OpenClaw agent database open` is normal. Takes about 1.5 minutes.

Read the answer once `kk-elapsed.txt` exists:

```bash
python3 -c "import json; d=json.load(open('$HOME/.cache/eca/kk-out.json')); \
  [print(p.get('text')) for p in d.get('result',d).get('payloads',[])]"
```

## If it fails — say so first, not in passing

It failed if `kk-out.json` is empty or missing, has no `payloads` text, or the job didn't finish
within about 4 minutes. When that happens:

1. Put a line **at the very top of the briefing**, before the summary: "⚠️ Kingkong gave no
   answer (reason). Everything below is from the journal, Jira, GitHub, Multica and the
   `kk-board` output; his conversation-only decisions and 'waiting on you' items are missing."
2. Keep going with the other sources. Do not wait on it or retry silently.
3. Offer a re-run; if Ethan says yes, use the same detached command above.

Never leave a missing Kingkong section implied or unmentioned. If a later answer reveals the
output was lost, tell Ethan plainly that it was lost and why.

## Prompt for the briefing agent (narrow)

> READ-ONLY request from Ethan's ECA assistant. Change nothing, message no one. I already have
> board state from kk-board, so do NOT list tickets or statuses. Since <last entry date> morning,
> tell me ONLY: (1) decisions or agreements made with Ethan in conversation that are not visible
> on the board or in GitHub, (2) what is waiting on Ethan right now (approvals, reviews,
> choices), (3) anywhere what Ethan told you disagrees with the board, Jira or GitHub. Name items
> by plain description with the ID in parens. Tag memory-only items (conversation). Under 150
> words. End with one line on what you could not see.

Section (1) — decisions only in conversation — is what no other source has. Its answers are
claims, not facts: verify state against GitHub and Jira, and confirm who owns each action.
