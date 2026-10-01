#! /usr/bin/env bash
set -euo pipefail

usage() {
  cat <<EOF
Usage: $(basename "$0") FILE

Normalise a Makefile written by 'ghc -M':
  - absolute dependency paths become <absolute-path>/<filename>
  - each block of "target : dependency" rules is sorted

Writes the output to FILE
EOF
}

case "${1:-}" in -h|--help) usage; exit 0 ;; esac
[ $# -eq 1 ] || { usage >&2; exit 1; }

tmp=$(mktemp "$1.XXXXXX")
trap 'rm -f "$tmp"' EXIT

sed -E 's#^(.* : )/.*/#\1<absolute-path>/#' "$1" |
  awk '
    / : / { print | "LC_ALL=C sort"; next }
    { close("LC_ALL=C sort"); print; fflush() }
    END { close("LC_ALL=C sort") }
  ' > "$tmp"
mv "$tmp" "$1"
