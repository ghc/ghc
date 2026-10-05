#! /usr/bin/env bash
set -euo pipefail

# Packages whose ids are rewritten to <pkg>-<VERSION>-<HASH>
pkgs='base|containers|template-haskell'

usage() {
  cat <<EOF
Usage: $(basename "$0") FILE...

Format one of the test -dep-json output file with jq and normalise it:
  - sort "includes" and remove system includes such as stdc-predef.
  - ids of the packages ${pkgs[*]} become <pkg>-<VERSION>-<HASH>

Writes the output to FILE again
EOF
}

case "${1:-}" in -h|--help) usage; exit 0 ;; esac
[ $# -ge 1 ] || { usage >&2; exit 1; }

format() {
  jq --sort-keys '
    (.. | objects | select(has("includes")) | .includes) |=
      (map(select(startswith("/") or test("^[A-Za-z]:[\\\\/]") | not)) | sort)
  ' "$1" |
    sed -E "s/($pkgs)-[0-9.]+(-[0-9a-zA-Z+]+)?(-[0-9a-zA-Z]+)?/\\1-<VERSION>-<HASH>/g"
}

for file in "$@"; do
  json=$(format "$file")
  printf '%s\n' "$json" > "$file"
done
