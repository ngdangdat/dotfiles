# Generate Documentation

Use the Task tool to spawn a documentation writer subagent.

## Instructions

Call the Task tool with:
- `subagent_type`: "general-purpose"
- `model`: "haiku"
- `description`: "Document $ARGUMENTS"
- `prompt`: Read the agent instructions from `.claude/agents/doc-writer.md` and generate documentation for: $ARGUMENTS

## Usage

- `/docs src/api/` - Document API endpoints
- `/docs UserService` - Document a class/module
- `/docs README` - Generate/update README
