#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if (( $# != 4 )); then
    echo "Usage: $0 OLD_GHC OLD_LIB NEW_GHC NEW_LIB" >&2
    echo "Use an old compiler with the same base interface version and no indexed proof evidence." >&2
    exit 2
fi
old=("$(realpath "$1")" "-B$(realpath "$2")")
new=("$(realpath "$3")" "-B$(realpath "$4")")
flags=(-O1 -package-env - -i -i. -fexternal-interpreter -dcore-lint -dstg-lint -fdiagnostics-color=never)
stripped=(-fomit-interface-pragmas -fno-write-if-simplified-core)
review=source-proof-branches-reviewed-v1
reason='Compiler now checks source proof dependencies by family branch'
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

for mode in native no-code; do
    make_flags=(-v1 --make -no-link -fno-enable-rewrite-rules)
    if [[ $mode == no-code ]]; then
        make_flags+=(-fno-code -fwrite-interface)
    fi
    for form in prelude home literal ordinary generated legacy; do
        dir="$work/$mode/$form"
        mkdir -p "$dir"
        cd "$dir"
        extension=ExperimentalDependentTypes
        selector=fmap
        method_import=
        rhs="'Just 'False"
        case $form in
            prelude) rhs="fmap not ('Just 'True)" ;;
            home)
                selector=translate
                method_import='import Method (Translate(translate))'
                rhs="translate not ('Just 'True)"
                cat >Method.hs <<'HS'
{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fobject-code -fno-omit-interface-pragmas -fwrite-source-equations #-}
module Method where

class Translate t where
    translate :: (a -> b) -> t a -> t b
    count :: t a -> Int

instance Translate Maybe where
    translate _ Nothing = Nothing
    translate f (Just a) = Just (f a)
    count _ = 1
HS
                ;;
            ordinary) extension=NoExperimentalDependentTypes ;;
        esac
        case $form in
            generated)
                cat >Origin.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Origin where

data Choice = LeftChoice | RightChoice

flipChoice :: Choice -> Choice
flipChoice LeftChoice = RightChoice
flipChoice RightChoice = LeftChoice
HS
                ;;
            legacy)
                cat >Origin.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module Origin (Image, Other) where

type family Image :: Maybe Bool where
    Image = fmap not ('Just 'True)

type Other = fmap not ('Just 'True)
HS
                ;;
            *)
                cat >Origin.hs <<HS
{-# LANGUAGE DataKinds, $extension, TypeFamilies #-}
module Origin (Image) where
$method_import

type family Image (flag :: Bool) :: Maybe Bool where
    Image 'True = $rhs
    Image 'False = 'Just 'False
HS
                ;;
        esac

        "${old[@]}" "${flags[@]}" "${stripped[@]}" "${make_flags[@]}" Origin.hs
        cp Origin.hi before.hi
        "${old[@]}" --show-iface before.hi >before.txt
        "${new[@]}" "${flags[@]}" "${stripped[@]}" "${make_flags[@]}" Origin.hs >first.log 2>&1
        cp Origin.hi first.hi
        "${new[@]}" --show-iface first.hi >first.txt
        "${new[@]}" "${flags[@]}" "${stripped[@]}" "${make_flags[@]}" Origin.hs >second.log 2>&1
        cmp first.hi Origin.hi
        if grep -Fq 'Compiling Origin' second.log; then
            cat second.log >&2
            exit 1
        fi

        case $form in
            ordinary|generated|legacy)
                cmp before.hi first.hi
                if grep -Eq 'Compiling Origin|source-proof-branches-reviewed-v1' first.log first.txt; then
                    cat first.log >&2
                    exit 1
                fi
                ;;
            *)
                grep -Fq "Compiling Origin [$reason]" first.log
                grep -Fq "$review" first.txt
                ;;
        esac
        case $form in
            prelude|home)
                grep -Fq 'source-proof-rule-branches' first.txt
                if diff -q <(grep 'ABI hash:' before.txt) <(grep 'ABI hash:' first.txt) >/dev/null; then
                    echo "$mode $form acquired proof evidence without changing the ABI" >&2
                    exit 1
                fi
                ;;
            *)
                diff -u <(grep 'ABI hash:' before.txt) <(grep 'ABI hash:' first.txt)
                ;;
        esac

        if [[ $form == prelude || $form == home ]]; then
            mkdir client
            cd client
            if [[ $form == home ]]; then cp ../Method.hi .; fi
            cat >Proof.hs <<HS
{-# LANGUAGE CPP, DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-orphans #-}
module Proof where

import Data.Type.Equality ((:~:)(Refl))
import Origin (Image)
$method_import

{-# RULES "change the checked method" [~1] forall f. $selector f (Just True) = Nothing #-}

#ifdef LITERAL
proof :: Image 'False :~: 'Just 'False
#else
proof :: Image 'True :~: 'Just 'False
#endif
proof = Refl
HS
            proof=("${new[@]}" "${flags[@]}" -v0 -fforce-recomp -fno-code -c Proof.hs)
            cp ../before.hi Origin.hi
            "${proof[@]}" -fenable-rewrite-rules
            cp ../first.hi Origin.hi
            if "${proof[@]}" -fenable-rewrite-rules >rejected.log 2>&1 ||
               ! grep -Eq '^Proof\.hs:[0-9]+:9: error: \[GHC-83605\]' rejected.log; then
                cat rejected.log >&2
                exit 1
            fi
            "${proof[@]}" -fenable-rewrite-rules -DLITERAL
            "${proof[@]}" -fno-enable-rewrite-rules
        fi
        printf '%s %s passed.\n' "$mode" "$form"
    done
done
