#!/usr/bin/env bash
set -euo pipefail

reference_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$reference_dir/../../.." && pwd)
ghc_binary=${1:-"$repo_root/_build/stage1/bin/ghc"}
upstream_source_dir=${2:-}
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/ghc-dependent-reference.XXXXXX")
printf 'Compiler: %s\nArtifacts: %s\n' "$ghc_binary" "$build_dir"
"$ghc_binary" --numeric-version

cat > "$build_dir/expected" <<'EOF'
(1,False,True,True)
()
naive-bridge-forces-input
EOF

for opt in 0 2; do
  output_dir="$build_dir/o$opt"
  mkdir -p "$output_dir"
  "$ghc_binary" "-O$opt" -dcore-lint -fforce-recomp \
    -odir "$output_dir" -hidir "$output_dir" \
    "$reference_dir/Oracle.hs" -o "$output_dir/oracle"
  "$output_dir/oracle" > "$output_dir/result"
  diff -u "$build_dir/expected" "$output_dir/result"
done

mkdir -p "$build_dir/reject"
if "$ghc_binary" -fno-code -fforce-recomp -outputdir "$build_dir/reject" \
    "$reference_dir/Reject.hs" > "$build_dir/reject.log" 2>&1; then
  printf 'Incorrectly accepted the mismatched Boolean index.\n' >&2
  exit 1
fi
rg -q 'Bool.*Int' "$build_dir/reject.log"

if [[ -n "$upstream_source_dir" ]]; then
  expected_blob=c6bf5970d85d28ed6edb4d887f14cfa127a01be2
  actual_blob=$(git hash-object "$upstream_source_dir/Data/Singletons.hs")
  if [[ "$actual_blob" != "$expected_blob" ]]; then
    printf 'Data.Singletons source does not match the pinned blob.\n' >&2
    exit 1
  fi
  cat > "$build_dir/library-expected" <<'EOF'
(1,False,True,True)
()
library-reification-is-strict
EOF
  for opt in 0 2; do
    output_dir="$build_dir/library-o$opt"
    mkdir -p "$output_dir"
    "$ghc_binary" "-O$opt" -dcore-lint -fforce-recomp -i"$upstream_source_dir" \
      -odir "$output_dir" -hidir "$output_dir" \
      "$reference_dir/LibraryOracle.hs" -o "$output_dir/oracle"
    "$output_dir/oracle" > "$output_dir/result"
    diff -u "$build_dir/library-expected" "$output_dir/result"
  done
fi

printf 'All requested singleton reference checks passed.\n'
