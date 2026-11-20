#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ $# -lt 2 || $# -gt 3 || (${3:-} != '' && ${3:-} != --ghci) ]]; then
    echo "Usage: bash $0 GHC LIBDIR [--ghci]" >&2
    exit 2
fi

docs=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
examples=$docs/examples
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

ghc=("$1" "-B$2" -v0 -package-env - -O0 -fexternal-interpreter
     -fomit-interface-pragmas -fno-write-if-simplified-core
     -dcore-lint -dstg-lint -fwrite-ide-info -Werror=inaccessible-code
     -i "-i$examples/packed-vector" "-i$examples/qualified-pi"
     "-i$examples/closed-evaluation" "-i$examples/open-evaluation")

"${ghc[@]}" --make -outputdir "$out/native" -hiedir "$out/hie" \
    "$docs/Showcase.hs" -o "$out/showcase"
"$out/showcase" | tee "$out/native.stdout"
diff -u "$docs/Showcase.stdout" "$out/native.stdout"
for module in Main PackedVecLibrary ClosedTableLibrary; do
    test -s "$out/hie/$module.hie"
done

if [[ ${3:-} == --ghci ]]; then
    "${ghc[@]}" --interactive -ignore-dot-ghci -fbyte-code \
        -outputdir "$out/interactive" -hiedir "$out/interactive-hie" \
        "*$docs/Showcase.hs" >"$out/interactive.stdout" <<'END'
main
END
    diff -u "$docs/Showcase.stdout" "$out/interactive.stdout"
fi
