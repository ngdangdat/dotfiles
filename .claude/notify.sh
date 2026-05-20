#!/bin/bash
# Reads Claude Code hook JSON from stdin and emits an enriched desktop
# notification. Invoked from .claude/settings.json with no arguments.

JSON=$(cat 2>/dev/null || true)

if [ "${CLAUDE_NOTIFY_DEBUG:-0}" = "1" ]; then
  {
    printf '\n===== %s =====\n' "$(date '+%Y-%m-%d %H:%M:%S')"
    printf 'argv: %s\n' "$*"
    printf 'env.TERM_PROGRAM=%s\n' "$TERM_PROGRAM"
    printf 'env.__CFBundleIdentifier=%s\n' "$__CFBundleIdentifier"
    printf 'stdin:\n'
    if [ -n "$JSON" ] && command -v jq >/dev/null 2>&1; then
      printf '%s' "$JSON" | jq . 2>/dev/null || printf '%s\n' "$JSON"
    else
      printf '%s\n' "$JSON"
    fi
  } >> "$HOME/.claude/notify-debug.log" 2>/dev/null
fi

jq_get() {
  if [ -z "$JSON" ] || ! command -v jq >/dev/null 2>&1; then
    return 0
  fi
  printf '%s' "$JSON" | jq -r "$1 // empty" 2>/dev/null
}

truncate_str() {
  local s="$1" max="${2:-60}"
  if [ "${#s}" -gt "$max" ]; then
    printf '%s…' "${s:0:$max}"
  else
    printf '%s' "$s"
  fi
}

EVENT=$(jq_get '.hook_event_name')
CWD=$(jq_get '.cwd')
MESSAGE=$(jq_get '.message')
TOOL_NAME=$(jq_get '.tool_name')

if [ -n "$CWD" ]; then
  PROJECT=$(basename "$CWD")
else
  PROJECT="Claude Code"
fi

case "$EVENT" in
  PermissionRequest)
    EMOJI="🛡️"
    case "$TOOL_NAME" in
      Bash)        FIELD=$(jq_get '.tool_input.command') ;;
      Read|Edit|Write|NotebookEdit)
                   FIELD=$(jq_get '.tool_input.file_path') ;;
      Grep)        FIELD=$(jq_get '.tool_input.pattern') ;;
      Glob)        FIELD=$(jq_get '.tool_input.pattern') ;;
      WebFetch)    FIELD=$(jq_get '.tool_input.url') ;;
      "")          FIELD="" ;;
      *)           FIELD=$(jq_get '.tool_input | tostring') ;;
    esac
    FIELD=$(printf '%s' "$FIELD" | tr '\n' ' ')
    if [ -n "$TOOL_NAME" ]; then
      BODY="Permission: ${TOOL_NAME}($(truncate_str "$FIELD" 60))"
    else
      BODY="Claude needs your permission"
    fi
    ;;
  Notification)
    EMOJI="🔔"
    if [ -n "$MESSAGE" ]; then
      BODY=$(truncate_str "$(printf '%s' "$MESSAGE" | tr '\n' ' ')" 140)
    else
      BODY="Claude needs your attention"
    fi
    ;;
  Stop)
    EMOJI="✅"
    LAST_MSG=$(jq_get '.last_assistant_message')
    if [ -n "$LAST_MSG" ]; then
      BODY=$(truncate_str "$(printf '%s' "$LAST_MSG" | tr '\n' ' ')" 140)
    else
      BODY="Task completed"
    fi
    ;;
  *)
    EMOJI="🔔"
    BODY="${MESSAGE:-needs attention}"
    ;;
esac

TITLE="${EMOJI} ${PROJECT}"

case "$(uname -s)" in
  Darwin)
    if [ -n "$__CFBundleIdentifier" ]; then
      APP="$__CFBundleIdentifier"
    elif [ "$TERM_PROGRAM" = "vscode" ]; then
      APP="com.microsoft.VSCode"
    elif [ "$TERM_PROGRAM" = "WezTerm" ]; then
      APP="com.github.wez.wezterm"
    elif [ "$TERM_PROGRAM" = "iTerm.app" ]; then
      APP="com.googlecode.iterm2"
    else
      APP="com.apple.Terminal"
      PID=$$
      for _ in $(seq 1 10); do
        PID=$(ps -p "$PID" -o ppid= 2>/dev/null | tr -d ' ')
        [ -z "$PID" ] || [ "$PID" -le 1 ] && break
        PROC=$(ps -p "$PID" -o comm= 2>/dev/null | xargs basename 2>/dev/null)
        case "$PROC" in
          *Code*)       APP="com.microsoft.VSCode";     break ;;
          *wezterm*)    APP="com.github.wez.wezterm";   break ;;
          *iTerm*)      APP="com.googlecode.iterm2";    break ;;
          *Warp*)       APP="dev.warp.Warp-Stable";     break ;;
          *Alacritty*)  APP="io.alacritty";             break ;;
          *kitty*)      APP="net.kovidgoyal.kitty";     break ;;
        esac
      done
    fi
    if [ "${DRY_RUN:-0}" = "1" ]; then
      printf 'title=%s\nbody=%s\napp=%s\n' "$TITLE" "$BODY" "$APP"
    else
      terminal-notifier -title "$TITLE" -message "$BODY" -activate "$APP"
    fi
    ;;
  Linux)
    if [ "${DRY_RUN:-0}" = "1" ]; then
      printf 'title=%s\nbody=%s\n' "$TITLE" "$BODY"
    else
      notify-send -a "Claude Code" "$TITLE" "$BODY" 2>/dev/null || true
    fi
    ;;
esac
