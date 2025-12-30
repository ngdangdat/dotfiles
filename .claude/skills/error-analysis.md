# Error Analysis Skill

Analyze error messages, stack traces, and exceptions to understand failures.

## Input
- Error message, stack trace, or exception via $ARGUMENTS

## Analysis Process

1. **Parse the error**
   - Identify error type/class
   - Extract error message
   - Parse stack trace frames

2. **Identify patterns**
   - Common error patterns (null pointer, type mismatch, etc.)
   - Framework-specific errors
   - Environment issues

3. **Trace the source**
   - Identify originating file and line
   - Understand call chain
   - Find the actual failure point vs. symptom

## Output Format

```md
### Error Type
[Classification of the error]

### Root Location
`file:line` - [Brief description]

### Error Chain
1. [First frame] - what happened
2. [Second frame] - what happened
...

### Likely Cause
[Explanation of what went wrong]

### Suggested Fix
[Concrete steps to resolve]
```
