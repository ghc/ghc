#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
area=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
base=("$1" "-B$2" -v0 -package-env - -i -i. -fexternal-interpreter -dcore-lint -dstg-lint)
cd "$work"
cp "$area"/NativeTypeCaseRetainedConTh{,Provider,Client,Wire}.hs .
cp "$area"/NativeTypeCaseRetainedReify.hs .

cat >BadScope.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TemplateHaskell #-}
module BadScope where

import GHC.Types (Multiplicity(One))
import Language.Haskell.TH
import qualified NativeTypeCaseRetainedConThProvider as P

$(do
  b <- newName "b"
  let parent = mkName "Bad"
      bang = Bang NoSourceUnpackedness NoSourceStrictness
      payload = GadtValueArg bang (PromotedT 'One) (AppT (ConT ''P.Select) (IndexVarT b))
      binder = GadtIndexArg b (ConT ''Bool)
  pure [DataD [] parent [] Nothing [RetainedGadtC [parent] [payload, binder] (ConT parent)] []])
HS
cat >BadBang.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TemplateHaskell #-}
module BadBang where

import GHC.Types (Multiplicity(One))
import Language.Haskell.TH

$(do
  b <- newName "b"
  let parent = mkName "Bad"
      payload = GadtValueArg (Bang NoSourceUnpackedness SourceStrict) (PromotedT 'One) (ConT ''Int)
      binder = GadtIndexArg b (ConT ''Bool)
  pure [DataD [] parent [] Nothing [RetainedGadtC [parent] [binder, payload] (ConT parent)] []])
HS

"${base[@]}" -package ghci -package template-haskell -package binary -package bytestring \
    NativeTypeCaseRetainedConThWire.hs -o wire
./wire
for opt in -O0 -O2; do
    ghc=("${base[@]}" "$opt" -fforce-recomp)
    "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeTypeCaseRetainedConThProvider.hs
    mv NativeTypeCaseRetainedConThProvider.hs provider.source
    "${ghc[@]}" -fomit-interface-pragmas -fno-write-if-simplified-core -c NativeTypeCaseRetainedConTh.hs
    mv NativeTypeCaseRetainedConTh.hs quoted.source
    "${ghc[@]}" -c NativeTypeCaseRetainedConThClient.hs
    "${ghc[@]}" NativeTypeCaseRetainedConThClient.o NativeTypeCaseRetainedConTh.o \
        NativeTypeCaseRetainedConThProvider.o -o run
    ./run >actual
    printf "(11,'x',27)\n(11,'x',27)\n29\n(False,\"True\",19,'q')\n" | diff -u - actual
    mv quoted.source NativeTypeCaseRetainedConTh.hs
    mv provider.source NativeTypeCaseRetainedConThProvider.hs
done

reject() {
    local file=$1 diagnostic=$2 status=0
    "${base[@]}" -c "$file.hs" >reject.log 2>&1 || status=$?
    if [[ $status != 1 ]] || ! grep -Fq "$diagnostic" reject.log; then
        cat reject.log >&2
        exit 1
    fi
}
reject NativeTypeCaseRetainedReify "Can't represent visible forall"
reject BadBang 'Unexpected strictness'
reject BadScope 'Malformed type IndexVarT'
