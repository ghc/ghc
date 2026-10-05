#! /usr/bin/env bash
set -euo pipefail

usage() {
  cat <<EOF2
Usage: $(basename "$0") FILE...

Normalise a Makefile written by 'ghc -M':
  - Absolute dependency paths become <absolute-path>/<filename>
  - Remove system dependencies as they are are not platform independent
  - The rules between the "DO NOT DELETE" markers are sorted

Writes the output to FILE
EOF2
}

case "${1:-}" in -h|--help) usage; exit 0 ;; esac
[ $# -ge 1 ] || { usage >&2; exit 1; }

# absolute paths need to be replaced with a placeholder.
absolute='^(.* : )(/|[A-Za-z]:[\\/])'

format() {
  # Drop absolute dependencies that are not interface files, i.e. CPP includes
  sed -E "\#$absolute#{ /hi(-boot)?\$/!d; }" "$1" |
    # Shorten the remaining absolute paths to <absolute-path>/<filename>
    sed -E "s#$absolute.*[\\/]#\\1<absolute-path>/#"
}

for file in "$@"; do
  # On failure, set -e stops before the file is overwritten
  makefile=$(format "$file")
  printf '%s\n' "$makefile" > "$file"
done
