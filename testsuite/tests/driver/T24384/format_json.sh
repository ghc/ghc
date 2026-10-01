#! /usr/bin/env bash
set -euo pipefail

# Packages whose ids are rewritten to <pkg>-<VERSION>-<HASH>
pkgs='base|containers|template-haskell'

usage() {
  cat <<EOF
Usage: $(basename "$0") FILE

Format one of the test -dep-json output file with jq and normalise it:
  - absolute paths in "includes" become <absolute-path>/<filename>,
    and the "includes" lists are sorted
  - ids of the packages ${packages[*]}
    (e.g. base-4.23.0.0-inplace) become <pkg>-<VERSION>-<HASH>

Writes the output to FILE
EOF
}

case "${1:-}" in -h|--help) usage; exit 0 ;; esac
[ $# -eq 1 ] || { usage >&2; exit 1; }

# Escape regex metacharacters (and the / delimiter) for sed
esc() { sed 's/[][\.*^$+?(){}|/]/\\&/g'; }

json=$(jq --sort-keys '
  (.. | objects | select(has("includes")) | .includes) |=
    (map(if startswith("/") then "<absolute-path>/" + (split("/") | last) else . end) | sort)
' "$1")

tmp=$(mktemp "$1.XXXXXX")
trap 'rm -f "$tmp"' EXIT
sed -E "s/($pkgs)-[0-9.]+(-[0-9a-zA-Z+]+)?(-[0-9a-zA-Z]+)?/\\1-<VERSION>-<HASH>/g" <<<"$json" > "$tmp"
mv "$tmp" "$1"
