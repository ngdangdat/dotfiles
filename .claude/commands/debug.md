# Debug Issue

Use the Task tool to spawn a debugger subagent.

## Instructions

Call the Task tool with:
- `subagent_type`: "general-purpose"
- `model`: "sonnet"
- `description`: "Debug $ARGUMENTS"
- `prompt`: Read the agent instructions from `.claude/agents/debugger.md` and debug the issue: $ARGUMENTS

## Usage

- `/debug Error: Cannot read property 'x' of undefined`
- `/debug The API returns 500 on login`
- `/debug Tests failing after recent changes`
