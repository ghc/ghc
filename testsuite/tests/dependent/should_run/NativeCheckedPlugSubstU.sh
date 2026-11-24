#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

inputs=$PWD
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fno-write-if-simplified-core -fdiagnostics-color=never)

for opt in -O0 -O2; do
    provider="$work/$opt/provider"
    mkdir -p "$provider" "$work/$opt"/{native,no-code,withheld}
    cd "$provider"
    cp "$inputs/NativeCheckedPlugSubstUProvider.hs" .
    "${ghc[@]}" "$opt" -fwrite-source-equations -fomit-interface-pragmas \
        -c NativeCheckedPlugSubstUProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedPlugSubstUProvider.hi >provider.iface
    for needle in 'SourceTable T:plug' 'SourceTable T:plugS' \
                  'CheckedClosureSourceTable T:substSingleU' \
                  'CheckedClosureSourceTable T:renameU [extU]' \
                  'SourceTable T:extU'; do
        grep -Fq "$needle" provider.iface || { echo "Missing $needle at $opt" >&2; cat provider.iface >&2; exit 1; }
    done

    cd "$work/$opt/no-code"
    cp "$provider/NativeCheckedPlugSubstUProvider.hi" "$inputs/NativeCheckedPlugSubstU.hs" .
    "${ghc[@]}" "$opt" -fno-code -fwrite-interface -c NativeCheckedPlugSubstU.hs
    test ! -f NativeCheckedPlugSubstUProvider.o
    test ! -f NativeCheckedPlugSubstU.o

    cd "$work/$opt/native"
    cp "$provider/NativeCheckedPlugSubstUProvider.hi" "$inputs/NativeCheckedPlugSubstU.hs" .
    "${ghc[@]}" "$opt" -c NativeCheckedPlugSubstU.hs
    "${ghc[@]}" "$opt" NativeCheckedPlugSubstU.o "$provider/NativeCheckedPlugSubstUProvider.o" -o run
    actual=$(./run)
    [[ "$actual" == '(Refl,Refl)' ]] || { echo "Unexpected ./run output: $actual" >&2; exit 1; }

    for bad in WRONG_HOLE WRONG_HOLE_SELF WRONG_LAM WRONG_APPR WRONG_CAPTURE_U WRONG_LOCAL_SUBST; do
        if "${ghc[@]}" "$opt" -fno-code -fforce-recomp "-D$bad" \
             -c NativeCheckedPlugSubstU.hs >wrong.log 2>&1 ||
           ! grep -Eq '\[GHC-(83865|25897|27958)\]' wrong.log; then
            echo "Failed negative control $bad at $opt:" >&2
            cat wrong.log >&2
            exit 1
        fi
    done

    for rbad in RULE_PLUG RULE_SUBST_U; do
        if "${ghc[@]}" "$opt" -fenable-rewrite-rules -fforce-recomp "-D$rbad" \
             -c NativeCheckedPlugSubstU.hs >rule.log 2>&1 ||
           ! grep -Fq '[GHC-83605]' rule.log; then
            echo "Failed rule control $rbad at $opt:" >&2
            cat rule.log >&2
            exit 1
        fi
    done

    cd "$work/$opt/withheld"
    cp "$inputs/NativeCheckedPlugSubstUProvider.hs" .
    for wbad in STRICT_PLUG CHANGED_PLUG HIDE_CTX; do
        "${ghc[@]}" "$opt" -fwrite-source-equations -fomit-interface-pragmas \
            -fforce-recomp "-D$wbad" -c NativeCheckedPlugSubstUProvider.hs
        "${ghc[@]}" --show-iface NativeCheckedPlugSubstUProvider.hi >withheld.iface
        if grep -Fq 'SourceTable T:plug$' withheld.iface || grep -Fq 'SourceTable T:plug ' withheld.iface || grep -Fxq '  [SourceTable T:plug]' withheld.iface || grep -Eq 'SourceTable T:plug([^S]|$)' withheld.iface; then
            echo "Unexpectedly published T:plug under $wbad at $opt" >&2
            exit 1
        fi
    done

    "${ghc[@]}" "$opt" -fwrite-source-equations -fomit-interface-pragmas \
        -fforce-recomp -DSTRICT_SUBST -c NativeCheckedPlugSubstUProvider.hs
    "${ghc[@]}" --show-iface NativeCheckedPlugSubstUProvider.hi >withheld.iface
    if grep -Fq 'CheckedClosureSourceTable T:substSingleU' withheld.iface; then
        echo "Unexpectedly published T:substSingleU under STRICT_SUBST at $opt" >&2
        exit 1
    fi
done

mkdir "$work/interactive"
cd "$work/interactive"
cp "$inputs/NativeCheckedPlugSubstUProvider.hs" "$inputs/NativeCheckedPlugSubstU.hs" .
"${ghc[@]}" --interactive -ignore-dot-ghci -fwrite-source-equations \
    NativeCheckedPlugSubstU.hs -e main >observed
grep -Fxq '(Refl,Refl)' observed || { cat observed >&2; exit 1; }
test ! -e NativeCheckedPlugSubstUProvider.o
