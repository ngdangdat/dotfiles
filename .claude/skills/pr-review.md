# PR Review Skill

Analyze pull request changes for code quality issues.

## Input
- PR number or diff content via $ARGUMENTS

## Analysis Areas
- **Code Quality**: Clean code principles, readability, maintainability
- **Logic & Correctness**: Potential bugs, edge cases, error handling
- **Performance**: Inefficient algorithms, unnecessary computations, memory issues
- **Testing**: Adequate test coverage, test quality
- **Documentation**: Clear comments where needed, updated docs if applicable

## Output Format

For each issue found, output:

```md
### What
[Describe the issue clearly]

### Why
[Explain why this is a problem]

### Suggestion
[Provide concrete fix or improvement]

**File**: `path/to/file:line_number`
**Severity**: Critical | Major | Minor | Nitpick
```

## Severity Levels
- **Critical**: Must fix before merge (bugs, breaking changes)
- **Major**: Should fix (significant code quality issues)
- **Minor**: Nice to fix (style, minor improvements)
- **Nitpick**: Optional suggestions
