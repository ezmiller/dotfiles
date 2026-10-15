# Engineering Manager (Kingkong)

Kingkong is the OpenClaw agent (`main`) on botserver. It runs the whole `primarykids` Multica
workspace and keeps the conversation with Ethan about that work. Use it as a *source of claims*,
never as the final word on state — git, then Jira, outrank it. It can't see work done outside
Multica, nor GitHub merge/CI state.

## Asking

```
ssh botserver-openclaw "node /home/openclaw/.local/opt/openclaw/dist/index.js agent --agent main \
  --session-id eca-<purpose>-<date> --timeout 400 --json --message '<prompt>'"
```

Use a fresh `--session-id` (keeps his main session clean) and no `--deliver` (reply returns to
you, nothing is sent to chat). Takes 2-5 min: run it in parallel with other gathering. Parse
`result.payloads[].text` from the JSON. If it fails or times out, continue without it and say so.

## Prompt for the briefing

Open with "READ-ONLY request from Ethan's ECA assistant. Change nothing, message no one."
Then: window (since the last journal entry date); use board/graph tools for state; name every
ticket/PR by plain description with the ID in parens; answer in sections — (A) landed or moved,
(B) decided in conversation, (C) waiting on Ethan, (D) disagreements (board vs Jira vs GitHub
vs what Ethan told you), (E) next; tag anything from conversation memory `(conversation)`;
under 300 words; end with what it couldn't see.

Ask at **epic/chain level**, not about one PR — a single-PR question misses the chain.
Section B (decisions only in conversation) is what no other source has.

## Tools it uses

`kk-board` (board state), `kk-graph <epic>` (plan/chains/dependencies), `kk-sync` (board vs Jira).
