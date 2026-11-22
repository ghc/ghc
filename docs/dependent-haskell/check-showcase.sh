#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

usage() {
    echo "Usage: bash $0 GHC LIBDIR [-O1 | -O2 | --default-o1 | --default-o2] [--ghci]" >&2
    exit 2
}
[[ $# -ge 2 ]] || usage
compiler=$1
libdir=$2
shift 2
native_opt=-O0
stripped_interfaces=(-fomit-interface-pragmas -fno-write-if-simplified-core)
native_interfaces=("${stripped_interfaces[@]}")
interactive=false
for option in "$@"; do
    case "$option" in
        -O1|-O2) native_opt=$option ;;
        --default-o1) native_opt=-O1; native_interfaces=() ;;
        --default-o2) native_opt=-O2; native_interfaces=() ;;
        --ghci) interactive=true ;;
        *) usage ;;
    esac
done

docs=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
examples=$docs/examples
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

ghc=("$compiler" "-B$libdir" -v0 -package-env - -fexternal-interpreter)
flags=(-dcore-lint -dstg-lint -fwrite-ide-info -Werror=inaccessible-code
     -i "-i$examples/packed-vector" "-i$examples/qualified-pi"
     "-i$examples/closed-evaluation" "-i$examples/open-evaluation"
     "-i$examples/erased-existentials" "-i$examples/r11-heq-pair")

"${ghc[@]}" "$native_opt" "${native_interfaces[@]}" "${flags[@]}" --make -outputdir "$out/native" -hiedir "$out/hie" \
    "$docs/Showcase.hs" -o "$out/showcase"
"$out/showcase" | tee "$out/native.stdout"
diff -u "$docs/Showcase.stdout" "$out/native.stdout"
for module in Main PackedVecLibrary ClosedTableLibrary ErasedPackage ForgetWitness ImplicitVector PackageEquality DefaultVector; do
    test -s "$out/hie/$module.hie"
done

mkdir "$out/indexed-client"
cp "$out/native"/{DirectIndexedVector,IndexedMatrix,PackedVecLibrary,PrimitiveWordFields}.hi "$out/indexed-client/"
cp "$examples/packed-vector"/{DirectIndexedVectorClient,IndexedMatrixClient}.hs \
    "$examples/open-evaluation/PrimitiveWordFieldsClient.hs" "$out/indexed-client/"
(
    cd "$out/indexed-client"
    for module in DirectIndexedVectorClient IndexedMatrixClient PrimitiveWordFieldsClient; do
        "${ghc[@]}" "$native_opt" "${native_interfaces[@]}" -i -i. -dcore-lint -dstg-lint -c "$module.hs"
    done
)

if "$interactive"; then
    "${ghc[@]}" -O0 "${stripped_interfaces[@]}" "${flags[@]}" --interactive -ignore-dot-ghci -fbyte-code \
        -outputdir "$out/interactive" -hiedir "$out/interactive-hie" \
        "*$docs/Showcase.hs" >"$out/interactive.stdout" <<'END'
main
END
    diff -u "$docs/Showcase.stdout" "$out/interactive.stdout"
fi
