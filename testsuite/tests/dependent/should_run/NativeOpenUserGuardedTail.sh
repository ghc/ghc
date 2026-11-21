#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 2
ghc+=("$@")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{provider,alias,client}
cat >"$out/provider/UserListTail.hs" <<'HS'
{-# OPTIONS_GHC -fwrite-source-equations #-}
module UserListTail (dropOneIf) where

dropOneIf :: (a -> Bool) -> [a] -> [a]
dropOneIf _ [] = []
dropOneIf p input@(x : xs)
    | p x = xs
    | otherwise = input
HS
cat >"$out/alias/TailAlias.hs" <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, Modifiers #-}
module TailAlias (Remainder, Negated) where

import GHC.Exts (U)
import qualified UserListTail as User

type Remainder :: (a %U -> Bool) %U -> [a] %U -> [a]
type Remainder p xs = User.dropOneIf p xs

type Negated :: Bool %U -> Bool
type Negated = not
HS
cat >"$out/client/Main.hs" <<'HS'
{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import TailAlias (Negated, Remainder)
import UserListTail (dropOneIf)

removed :: Proxy xs -> Remainder Negated ('False ': xs) :~: xs
removed _ = Refl

preserved :: Proxy xs -> Remainder Negated ('True ': xs) :~: ('True ': xs)
preserved _ = Refl

main :: IO ()
main = do
    demand <- try (evaluate (dropOneIf (const True) (undefined : error "selected tail" :: [Bool])))
    print ( removed (Proxy @'[]), preserved (Proxy @'[])
          , take 1 (dropOneIf (const True) (undefined : True : error "forced later tail"))
          , hasCons (dropOneIf (const False) (undefined : undefined :: [Bool]))
          , case demand of Left (ErrorCall "selected tail") -> True; _ -> False )
  where
    hasCons (_ : _) = True
    hasCons [] = False
HS
stripped=(-fomit-interface-pragmas -fno-write-if-simplified-core)
for level in -O0 -O2; do
    cd "$out/provider"
    "${ghc[@]}" "$level" "${stripped[@]}" -fforce-recomp -c UserListTail.hs
    "${ghc[@]}" --show-iface UserListTail.hi >owner.iface
    grep -Fq 'FinalCoreSourceTable T:dropOneIf' owner.iface
    cd "$out/alias"
    cp ../provider/UserListTail.hi .
    "${ghc[@]}" "$level" "${stripped[@]}" -fforce-recomp -c TailAlias.hs
    cd "$out/client"
    cp ../alias/{TailAlias,UserListTail}.hi .
    "${ghc[@]}" "$level" -fforce-recomp -c Main.hs
    "${ghc[@]}" Main.o ../provider/UserListTail.o ../alias/TailAlias.o -o run
    [[ $(./run) == '(Refl,Refl,[True],True,True)' ]]
done

# An extra demand on a head prevents publication, even when the result is the tail.
cd "$out/provider"
sed 's/| p x = xs/| p x = x `seq` xs/' UserListTail.hs >Strict.hs
mv Strict.hs UserListTail.hs
"${ghc[@]}" -O2 "${stripped[@]}" -fforce-recomp -c UserListTail.hs
"${ghc[@]}" --show-iface UserListTail.hi >strict.iface
if grep -Eq 'T:(#guard:)?dropOneIf' strict.iface; then
    cat strict.iface >&2
    exit 1
fi
cd "$out/alias"
cp ../provider/UserListTail.hi .
line=$(awk '/^type Remainder p xs =/ { print NR }' TailAlias.hs)
if "${ghc[@]}" -O2 -fforce-recomp -c TailAlias.hs >strict.log 2>&1 ||
   ! grep -Fq "TailAlias.hs:$line:" strict.log ||
   ! grep -Fq '[GHC-70565]' strict.log ||
   ! grep -Fq 'User.dropOneIf p xs' strict.log; then
    cat strict.log >&2
    exit 1
fi
