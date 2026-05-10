#!/bin/bash
TITLE="$1"
MESSAGE="$2"

case "$(uname -s)" in
  Darwin)
    if [ "$TERM_PROGRAM" = "vscode" ]; then
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
    terminal-notifier -title "$TITLE" -message "$MESSAGE" -activate "$APP"
    ;;
  Linux)
    notify-send -a "Claude Code" "$TITLE" "$MESSAGE" 2>/dev/null || true
    ;;
esac
