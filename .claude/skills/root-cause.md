# Root Cause Analysis Skill

Deep investigation to find the underlying cause of issues.

## Input
- Bug description, symptoms, or failing behavior via $ARGUMENTS

## Investigation Process

1. **Reproduce understanding**
   - What are the exact symptoms?
   - When does it occur?
   - What changed recently?

2. **Hypothesis generation**
   - List possible causes
   - Rank by likelihood
   - Identify testable predictions

3. **Evidence gathering**
   - Search codebase for related code
   - Check git history for recent changes
   - Look for similar patterns

4. **Verification**
   - Confirm hypothesis with evidence
   - Rule out alternatives

## Output Format

```md
### Symptoms
[Observable behavior]

### Investigation
[Steps taken and findings]

### Root Cause
[The underlying issue]

### Evidence
- [Evidence 1]
- [Evidence 2]

### Fix Recommendation
[How to address the root cause]

### Prevention
[How to prevent recurrence]
```
