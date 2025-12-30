# Run Tests

Use the Task tool to spawn a test-runner subagent.

## Instructions

Call the Task tool with:
- `subagent_type`: "general-purpose"
- `model`: "haiku"
- `description`: "Run tests $ARGUMENTS"
- `prompt`: Read the agent instructions from `.claude/agents/test-runner.md` and run tests. Scope: $ARGUMENTS (if empty, run all tests)

## Usage

- `/test` - Run all tests
- `/test src/utils` - Test specific directory
- `/test --coverage` - Run with coverage
