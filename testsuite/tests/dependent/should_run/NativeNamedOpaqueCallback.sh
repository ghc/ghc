#!/usr/bin/env bash
set -euo pipefail

src=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never -ferror-spans)
fixture=NativeNamedOpaqueCallback

reject() {
    if "${ghc[@]}" "${flags[@]}" -fforce-recomp -c "$@" > reject.log 2>&1 ||
       ! grep -Fq '[GHC-83865]' reject.log; then
        cat reject.log >&2
        exit 1
    fi
}

for opt in 0 2; do
    flags=("-O$opt" -fomit-interface-pragmas -fno-write-if-simplified-core)
    mkdir -p "$work/$opt"/{provider,writer,reader}
    cd "$work/$opt/provider"
    cp "$src/""${fixture}"{Provider,Alias}.hs .
    "${ghc[@]}" "-O$opt" -fno-omit-interface-pragmas -c "${fixture}Provider.hs"
    "${ghc[@]}" --show-iface "${fixture}Provider.hi" > provider.iface
    grep -Eq 'Inline:|InlineRule' provider.iface
    "${ghc[@]}" "${flags[@]}" -c "${fixture}Alias.hs"
    cp ./*.hi ../writer/

    cd ../writer
    cp "$src/${fixture}Public.hs" .
    "${ghc[@]}" "${flags[@]}" -c "${fixture}Public.hs"
    "${ghc[@]}" --show-iface "${fixture}Public.hi" > public.iface
    grep -Eq '^         got +[0-9]+-retained-features-v1-named-retained-callback-v1$' public.iface
    grep -q "Stored ${fixture}Provider.mapper" public.iface
    grep -q "Call (${fixture}Provider.mapper x)" public.iface
    cp ./*.hi ../reader/
    reject "${fixture}Public.hs" -DWRONG_LEXICAL_BINDER

    cd ../reader
    cp "$src/${fixture}Client.hs" Main.hs
    "${ghc[@]}" "${flags[@]}" -c Main.hs
    reject Main.hs -DWRONG_OWNER
    reject Main.hs -DWRONG_CALL
    "${ghc[@]}" Main.o ../provider/*.o ../writer/*.o -o run
    ./run > result
    printf '(True,True)\n' > expected
    diff -u expected result
done
