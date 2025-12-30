#!/bin/bash

input=$(cat)
# design
# M:<model> | D:<current_dir> | CTX: <token_in>I <token out>O | LST <token_in>I O<token out>O

# Helper functions for common extractions
# Ref: https://code.claude.com/docs/en/statusline
get_model_id() { echo "$input" | jq -r '.model.id'; }
get_current_dir() { echo "$input" | jq -r '.workspace.current_dir'; }
get_project_dir() { echo "$input" | jq -r '.workspace.project_dir'; }
get_version() { echo "$input" | jq -r '.version'; }
get_cost() { echo "$input" | jq -r '.cost.total_cost_usd'; }
get_duration() { echo "$input" | jq -r '.cost.total_duration_ms'; }
get_input_tokens() { echo "$input" | jq -r '.context_window.total_input_tokens'; }
get_output_tokens() { echo "$input" | jq -r '.context_window.total_output_tokens'; }
get_cu_input_tokens() { echo "$input" | jq -r '.context_window.current_usage.input_tokens // 0'; }
get_cu_output_tokens() { echo "$input" | jq -r '.context_window.current_usage.output_tokens // 0'; }
get_context_window_size() { echo "$input" | jq -r '.context_window.context_window_size'; }

GIT_BRANCH="NOPE"
if git rev-parse --git-dir > /dev/null 2>&1; then
    BRANCH=$(git branch --show-current 2>/dev/null)
    if [ -n "$BRANCH" ]; then
        GIT_BRANCH="🌿 $BRANCH"
    fi
fi


# echo M:$(get_model_id)
echo "GIT>${GIT_BRANCH} | DIR>$(get_current_dir) | MOD>$(get_model_id) | CTX>$(get_input_tokens) in $(get_output_tokens) out | LST>$(get_cu_input_tokens) in $(get_cu_output_tokens) out"
