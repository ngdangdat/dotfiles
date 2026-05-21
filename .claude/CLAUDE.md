# Claude Code Rules

Project-specific rules for Claude Code sessions.

## Rules

@rules/git.md
@rules/communication.md
@rules/constraints.md

## Personal LLM Wiki

A personal knowledge wiki lives at `$BRAIN/second-brain-obsidian-vault/` (the vault repo cloned on this machine; see `$BRAIN` env var).

**When to consult it.** Before answering, when the user references:
- Past work, prior decisions, or "what was I doing on X / around Y"
- Project status ("where's project Z?", "what's open on Z?")
- People, topics, or threads that may live in the wiki

**How to consult it.** Prefer the `wiki_search` MCP tool when available — call it with the user's query and read the returned page(s).

If `wiki_search` is unavailable on this machine, fall back to direct file access:
- `index.md` — catalog of project pages with one-line current statuses
- `projects/<Name>.md` — per-project current Status, Open Threads, Recent Decisions, Activity
- `daily-notes/YYYY-MM-DD.md` — raw daily notes (immutable; written by the user)
- `log.md` — chronological ingest/query record
- `AGENTS.md` — vault schema (read first if you don't recognize a heading or alias)

If `$BRAIN` is unset on this machine, surface the missing env var to the user — do not guess a path or hallucinate wiki content.

**Write discipline.** Do **not** modify any wiki file from outside the vault repo. Project pages, `index.md`, and `log.md` are only ever written by the ingest workflow defined in `$BRAIN/second-brain-obsidian-vault/AGENTS.md` → `## Ingest Workflow`, triggered by the user from inside the vault repo. If the user asks you to update wiki content while you're in a non-vault project, decline and direct them to run the ingest workflow from the vault.
