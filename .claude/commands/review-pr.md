# Review Pull Request

Use the Task tool to spawn a code reviewer subagent.

## Instructions

Call the Task tool with:
- `subagent_type`: "general-purpose"
- `model`: "sonnet"
- `description`: "Review PR $ARGUMENTS"
- `prompt`: Read the agent instructions from `.claude/agents/code-reviewer.md` and review PR $ARGUMENTS. If no PR specified, use the current branch's PR.

## Usage

Invoke with PR number or URL:
- `/review-pr 123`
- `/review-pr https://github.com/owner/repo/pull/123`

If no argument provided, review the current branch's PR.
