#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -v0 -package-env - -i -dcore-lint -dstg-lint
     -fomit-interface-pragmas -fno-write-if-simplified-core -fdiagnostics-color=never)
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT

for opt in 0 2; do
    for form in plain mixed mixed-nocode local local-nocode; do
        providers="$out/$opt/$form/providers"
        interfaces="$out/$opt/$form/interfaces"
        reader="$out/$opt/$form/reader"
        flags=(-O"$opt")
        provider_flags=()
        if [[ $form != plain ]]; then
            provider_flags=(-DMIXED_PROVIDER -XDataKinds -XExperimentalDependentTypes -XGADTs -XTypeOperators)
        fi
        if [[ $form == local* ]]; then
            provider_flags+=(-DLOCAL_PROOF)
        fi
        if [[ $form == *-nocode ]]; then
            flags+=(-fno-code -fwrite-interface)
        fi
        mkdir -p "$providers" "$interfaces" "$reader"
        cd "$providers"
        cp "$src/"{PreludeMapLengthSize,NativeOpenMonusSource}.hs .
        "${ghc[@]}" "${flags[@]}" "${provider_flags[@]}" -i. --make -no-link \
            -outputdir "$interfaces" NativeOpenMonusSource.hs
        "${ghc[@]}" --show-iface "$interfaces/NativeOpenMonusSource.hi" >source.iface
        if [[ $form == local* ]]; then
            grep -Fq '  SourceTable T:monus' source.iface
        else
            grep -Fq 'FinalCoreSourceTable T:monus' source.iface
        fi

        cd "$reader"
        cp "$src/NativeOpenMonus.hs" .
        "${ghc[@]}" "${flags[@]}" "-i$interfaces" -c NativeOpenMonus.hs
        if [[ $form == *-nocode ]]; then
            test ! -e "$interfaces/NativeOpenMonusSource.o" && test ! -e NativeOpenMonus.o
        else
            "${ghc[@]}" NativeOpenMonus.o "$interfaces/"{NativeOpenMonusSource,PreludeMapLengthSize}.o -o check
            [[ $(./check) == '(True,True,True,True)' ]]
        fi
        for error in WRONG_SUCCESSORS UNKNOWN_RIGHT; do
            if "${ghc[@]}" "${flags[@]}" -fforce-recomp "-i$interfaces" -D"$error" -c NativeOpenMonus.hs >error.log 2>&1 ||
               ! grep -Fq 'error: [GHC-83865]' error.log; then
                cat error.log >&2
                exit 1
            fi
        done
    done
done
