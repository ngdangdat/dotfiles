# Test Execution Skill

Run tests and analyze results.

## Input
- Test scope (all, specific file, pattern) via $ARGUMENTS

## Process

1. **Detect test framework**
   - Check package.json for jest, mocha, vitest
   - Check for pytest, unittest
   - Check for cargo test, go test
   - Look for test scripts

2. **Run tests**
   - Execute appropriate test command
   - Capture output and exit code
   - Note timing information

3. **Parse results**
   - Count passed/failed/skipped
   - Extract failure details
   - Identify flaky tests

## Output Format

```md
### Test Summary
- **Passed**: X
- **Failed**: Y
- **Skipped**: Z
- **Duration**: Xs

### Failed Tests
| Test | Error | Location |
|------|-------|----------|
| ... | ... | ... |

### Failure Details
[For each failure, show error and relevant context]

### Recommendations
[Suggested fixes or next steps]
```
