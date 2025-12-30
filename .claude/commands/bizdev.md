# Business Development

Use the Task tool to spawn a business development expert subagent.

## Instructions

Call the Task tool with:
- `subagent_type`: "general-purpose"
- `model`: "opus"
- `description`: "Bizdev $ARGUMENTS"
- `prompt`: Read the agent instructions from `.claude/agents/bizdev.md` and help with: $ARGUMENTS

## Usage

- `/bizdev Analyze SaaS market for developer tools`
- `/bizdev Write proposal for enterprise client`
- `/bizdev Competitive analysis for our product`
