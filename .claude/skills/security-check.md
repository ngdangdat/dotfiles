# Security Check Skill

Analyze code changes for security vulnerabilities.

## Input
- PR number or diff content via $ARGUMENTS

## Security Analysis Areas

### Input Validation
- Unsanitized user input
- Missing validation on API endpoints
- Type coercion issues

### Injection Vulnerabilities
- SQL injection
- Command injection
- XSS (Cross-Site Scripting)
- Template injection
- Path traversal

### Authentication & Authorization
- Missing auth checks
- Privilege escalation risks
- Session handling issues
- Insecure token handling

### Data Exposure
- Sensitive data in logs
- Hardcoded secrets/credentials
- Insecure data transmission
- PII exposure risks

### Dependencies
- Known vulnerable packages
- Outdated dependencies with CVEs

## Output Format

For each vulnerability found:

```md
### What
[Describe the vulnerability]

### Why
[Explain the security risk and potential impact]

### Suggestion
[Provide secure alternative or fix]

**File**: `path/to/file:line_number`
**Severity**: Critical | High | Medium | Low
**CWE**: [CWE ID if applicable]
```

## Severity Levels
- **Critical**: Immediate exploitation risk, data breach potential
- **High**: Significant security weakness
- **Medium**: Security concern requiring attention
- **Low**: Minor security improvement
