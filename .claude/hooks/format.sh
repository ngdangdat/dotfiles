#!/bin/bash
# Auto-format after file writes
#
# Required formatters (install as needed):
#   Python:     pip install black
#   JS/TS:      npm install -g prettier
#   Rust:       rustup component add rustfmt
#   Go:         (included with Go installation)
#
# Formatters are optional - missing ones are silently skipped.

FILE="$CLAUDE_TOOL_ARG_file_path"
case "$FILE" in
  *.py) black "$FILE" 2>/dev/null || true ;;
  *.js|*.ts|*.tsx) prettier --write "$FILE" 2>/dev/null || true ;;
  *.rs) rustfmt "$FILE" 2>/dev/null || true ;;
  *.go) gofmt -w "$FILE" 2>/dev/null || true ;;
esac
