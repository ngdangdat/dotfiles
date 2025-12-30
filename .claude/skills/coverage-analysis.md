# Coverage Analysis Skill

Analyze test coverage and identify gaps.

## Input
- Coverage report or scope via $ARGUMENTS

## Process

1. **Generate coverage**
   - Run tests with coverage enabled
   - Collect coverage data

2. **Analyze coverage**
   - Overall percentage
   - Per-file breakdown
   - Uncovered lines/branches

3. **Identify gaps**
   - Critical uncovered code paths
   - High-risk uncovered areas
   - Easy wins for coverage improvement

## Output Format

```md
### Coverage Summary
- **Lines**: X%
- **Branches**: Y%
- **Functions**: Z%

### Low Coverage Files
| File | Coverage | Uncovered Lines |
|------|----------|-----------------|
| ... | ... | ... |

### Critical Gaps
[Important code paths without tests]

### Recommendations
[Priority areas to add tests]
```
