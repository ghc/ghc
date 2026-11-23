#!/usr/bin/env bash
set -euo pipefail
src=$(cd "$(dirname "$0")" && pwd)
run=$(mktemp -d)
trap 'rm -rf -- "$run"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fomit-interface-pragmas -fno-write-if-simplified-core -fdiagnostics-color=never)
cd "$run"

for opt in 0 2; do
    flags=("-O$opt")
    mkdir -p "$run/$opt"/{provider,alias,exporter,reader}
    cd "$run/$opt/provider"
    cp "$src/CharacterCase.hs" .
    "${ghc[@]}" "${flags[@]}" -c CharacterCase.hs
    "${ghc[@]}" --show-iface CharacterCase.hi > provider.iface
    grep -Fq 'SourceTable T:flag' provider.iface
    grep -Fq 'SourceTable T:flagAlias' provider.iface
    grep -Fq 'SourceTable T:upperAlias' provider.iface
    cp CharacterCase.hi ../alias/
    cat >Uncertified.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Uncertified where
bad :: Char -> Bool
bad 'x' = True
bad _ = error "default is partial"

constant :: Char -> Bool
constant _ = True
HS
    "${ghc[@]}" "${flags[@]}" -c Uncertified.hs
    "${ghc[@]}" --show-iface Uncertified.hi > uncertified.iface
    ! grep -Eq 'SourceTable T:(bad|constant)' uncertified.iface

    cd ../alias
    cat >Forward.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes #-}
module Forward (flag, alias) where
import CharacterCase (flag)
alias :: Char -> Bool
alias = flag
HS
    "${ghc[@]}" "${flags[@]}" -c Forward.hs
    "${ghc[@]}" --show-iface Forward.hi > forward.iface
    grep -Fq 'SourceTable T:alias' forward.iface
    cp ./*.hi ../exporter/

    cd ../exporter
    cp "$src/CharacterExport.hs" .
    "${ghc[@]}" "${flags[@]}" -c CharacterExport.hs
    cat >AliasExport.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module AliasExport (Alias, Reexport, LocalFlag, LocalUpper) where
import CharacterCase (flagAlias, upperAlias)
import Forward (alias, flag)
type Alias (c :: Char) = alias c
type Reexport (c :: Char) = flag c
type LocalFlag (c :: Char) = flagAlias c
type LocalUpper (c :: Char) = upperAlias c
HS
    "${ghc[@]}" "${flags[@]}" -c AliasExport.hs
    cp ./*.hi ../reader/

    cd ../reader
    cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
module Proof where
import CharacterExport (Flag)
import AliasExport (Alias, Reexport, LocalFlag, LocalUpper)
import Data.Type.Equality ((:~:) (Refl))
literal :: Flag 'x' :~: 'True
literal = Refl

aliasLiteral :: Alias 'x' :~: 'True
aliasLiteral = Refl

reexported :: Reexport 'é' :~: 'True
reexported = Refl

fallback :: Alias '!' :~: 'False
fallback = Refl

localFlag :: LocalFlag 'x' :~: 'True
localFlag = Refl

localUpper :: LocalUpper 'x' :~: 'X'
localUpper = Refl
HS
    "${ghc[@]}" "${flags[@]}" -c Proof.hs
    "${ghc[@]}" "${flags[@]}" -c Proof.hs -v1 > warm.log 2>&1
    grep -Fxq 'compilation IS NOT required' warm.log

    cat >Rejected.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module Rejected where
import CharacterExport (Flag)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:) (Refl))
type family Classify (b :: Bool) :: Bool where
    Classify 'True = 'True
    Classify _ = 'False

wrong :: Flag 'x' :~: 'False
wrong = Refl

unknown :: Proxy c -> Flag c :~: 'False
unknown _ = Refl

catchAll :: Proxy c -> Classify (Flag c) :~: 'False
catchAll _ = Refl
HS
    if "${ghc[@]}" "${flags[@]}" -c Rejected.hs > reject.log 2>&1 ||
       [[ $(grep -Fc '[GHC-83865]' reject.log) != 3 ]]; then
        cat reject.log >&2
        exit 1
    fi

    cat >Main.hs <<'HS'
module Main where
import CharacterCase (flag)
import Control.Exception (ErrorCall, evaluate, try)
main :: IO ()
main = do
    outcome <- try (evaluate (flag (error "char demand sentinel"))) :: IO (Either ErrorCall Bool)
    case outcome of
      Left e | "char demand sentinel" `elem` lines (show e) -> pure ()
      _ -> error "flag did not demand the character"
HS
    "${ghc[@]}" "${flags[@]}" -c Main.hs
    "${ghc[@]}" Main.o ../provider/CharacterCase.o -o strictness
    ./strictness

    # Replace only the provider interface visible to the unchanged reader.
    cd ../provider
    sed "s/flag 'x' = True/flag 'x' = False/" CharacterCase.hs > adjusted
    mv adjusted CharacterCase.hs
    "${ghc[@]}" "${flags[@]}" -fforce-recomp -c CharacterCase.hs
    cp CharacterCase.hi ../reader/
    cd ../reader
    if "${ghc[@]}" "${flags[@]}" -c Proof.hs -ddump-hi-diffs > changed.log 2>&1 ||
       ! grep -Fq '[GHC-83865]' changed.log ||
       [[ $(grep -Fc '[GHC-83865]' changed.log) != 3 ]]; then
        cat changed.log >&2
        exit 1
    fi
done
