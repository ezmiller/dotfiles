#!/usr/bin/env bash
#
# Build agent instruction files for tools that don't support includes.
# Concatenates base.md + tool-specific.md → dist/OUTPUT.md
#
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DIST_DIR="${SCRIPT_DIR}/dist"

mkdir -p "${DIST_DIR}"

# Claude Code: base.md + claude-code.md → dist/CLAUDE.md
echo "Building CLAUDE.md..."
cat "${SCRIPT_DIR}/base.md" "${SCRIPT_DIR}/claude-code.md" > "${DIST_DIR}/CLAUDE.md"

# Codex: base.md + codex.md → dist/CODEX.md (when codex.md exists)
if [[ -f "${SCRIPT_DIR}/codex.md" ]]; then
    echo "Building CODEX.md..."
    cat "${SCRIPT_DIR}/base.md" "${SCRIPT_DIR}/codex.md" > "${DIST_DIR}/CODEX.md"
fi

echo "Agent files built in ${DIST_DIR}/"
