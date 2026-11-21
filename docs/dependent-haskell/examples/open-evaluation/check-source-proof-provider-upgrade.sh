#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
version_change=false
if [[ ${1-} == --different-interface-versions ]]; then
    version_change=true
    shift
fi
if (( $# != 4 )); then
    echo "Usage: $0 [--different-interface-versions] OLD_GHC OLD_LIB NEW_GHC NEW_LIB" >&2
    exit 2
fi
old=$(realpath "$1")
old_lib=$(realpath "$2")
new=$(realpath "$3")
new_lib=$(realpath "$4")
flags=(-O2 -v0 -package-env - -i -i. -fexternal-interpreter -dcore-lint -dstg-lint -fdiagnostics-color=never)
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"
mkdir provider client
cat > provider/Origin.hs <<'HS'
{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module Origin (Image) where
#ifdef FAMILY
type family Image where
    Image = Prelude.fmap Prelude.not ('Just 'True)
#else
type Image = Prelude.fmap Prelude.not ('Just 'True)
#endif
HS
cat > client/Proof.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-orphans #-}
module Proof where
import Data.Type.Equality ((:~:)(Refl))
import Origin (Image)

{-# RULES "change checked fmap" [~1] forall f. fmap f (Just True) = Nothing #-}

image :: Image :~: 'Just 'False
image = Refl
HS

build_provider() {
    (cd provider && "$1" "-B$2" "${flags[@]}" -fforce-recomp -fno-enable-rewrite-rules \
        -fomit-interface-pragmas -fno-write-if-simplified-core -fno-code -fwrite-interface -D"$3" -c Origin.hs)
    cp provider/Origin.hi client/
}

compile_proof() {
    (cd client && "$new" "-B$new_lib" "${flags[@]}" -fforce-recomp "$@" -c Proof.hs)
}

for form in ALIAS FAMILY; do
    build_provider "$old" "$old_lib" "$form"
    if "$version_change"; then
        if compile_proof -fenable-rewrite-rules > old-interface.log 2>&1 ||
           ! grep -Fq 'Origin.hi' old-interface.log ||
           ! grep -Fq 'mismatched interface file versions' old-interface.log; then
            cat old-interface.log >&2
            exit 1
        fi
    else
        compile_proof -fenable-rewrite-rules
    fi
    build_provider "$new" "$new_lib" "$form"
    if compile_proof -fenable-rewrite-rules > rejected.log 2>&1 ||
       ! grep -Fq 'Proof.hs:10:9: error: [GHC-83605]' rejected.log; then
        cat rejected.log >&2
        exit 1
    fi
    compile_proof -fno-enable-rewrite-rules
    printf '%s proof rejected after rebuilding its provider.\n' "$form"
done
