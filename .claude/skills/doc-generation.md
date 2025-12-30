# Documentation Generation Skill

Generate clear, useful documentation from code.

## Input
- File, function, or module to document via $ARGUMENTS

## Process

1. **Analyze code**
   - Understand purpose and functionality
   - Identify public API
   - Note dependencies and side effects

2. **Extract information**
   - Parameters and return types
   - Usage patterns
   - Edge cases and limitations

3. **Generate docs**
   - Clear description
   - Usage examples
   - API reference

## Output Format

```md
### Overview
[What this code does and why]

### Usage
\`\`\`
[Example code]
\`\`\`

### API Reference

#### `functionName(params)`
- **Parameters**:
  - `param1` (type) - description
- **Returns**: type - description
- **Throws**: ErrorType - when

### Examples
[Real-world usage examples]

### Notes
[Important considerations, gotchas]
```
