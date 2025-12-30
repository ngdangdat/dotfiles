# API Documentation Skill

Generate API documentation for endpoints.

## Input
- API endpoint or service to document via $ARGUMENTS

## Process

1. **Discover endpoints**
   - Find route definitions
   - Identify HTTP methods
   - Extract path parameters

2. **Analyze each endpoint**
   - Request format (headers, body, params)
   - Response format
   - Authentication requirements
   - Error responses

3. **Generate documentation**
   - OpenAPI-style format
   - Include examples
   - Note rate limits, versioning

## Output Format

```md
### Endpoint: `METHOD /path`

**Description**: What this endpoint does

**Authentication**: Required/Optional - type

**Request**
- Headers: `Header-Name: value`
- Path params: `:id` - description
- Query params: `?param=value` - description
- Body:
\`\`\`json
{ "field": "type" }
\`\`\`

**Response**
- `200 OK`
\`\`\`json
{ "result": "type" }
\`\`\`
- `400 Bad Request` - when
- `401 Unauthorized` - when

**Example**
\`\`\`bash
curl -X METHOD /path -H "Auth: token" -d '{}'
\`\`\`
```
