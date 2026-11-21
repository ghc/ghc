#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C.UTF-8
if (( $# != 4 )); then
    echo "Usage: $0 OLD_GHC OLD_LIB NEW_GHC NEW_LIB" >&2
    exit 2
fi
old=("$(realpath "$1")" "-B$(realpath "$2")")
new=("$(realpath "$3")" "-B$(realpath "$4")")
old_lib=$(realpath "$2")
old_pkg=$(realpath "$old_lib/../bin/ghc-pkg")
flags=(-O2 -v1 -package-env - -i -i. -dcore-lint -dstg-lint -fdiagnostics-color=never)
reason='Compiler now tracks home rule dependencies for final Core equations'
field='home-rule-closures-reviewed-v1'
package=r16-late-fold-0.1
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
mkdir "$work"/{library,db,rules-on,rules-off,ordinary}
cd "$work/library"
cat > LocalFold.hs <<'HS'
{-# OPTIONS_GHC -fwrite-source-equations #-}
module LocalFold (fold) where

{-# NOINLINE fold #-}
fold :: (a -> b -> b) -> b -> [a] -> b
fold _ seed [] = seed
fold step seed (x : xs) = step x (fold step seed xs)
HS
cat > HiddenStep.hs <<'HS'
module HiddenStep (delayed) where
import LocalFold (fold)

{-# NOINLINE lateStep #-}
lateStep :: Bool -> Bool -> Bool
lateStep _ r = r

{-# INLINE delayed #-}
delayed :: Bool -> [Bool] -> Bool
delayed seed xs = fold lateStep seed xs

{-# RULES "late/fold-empty" [0] fold lateStep False [] = True #-}
HS
cat > FoldBridge.hs <<'HS'
module FoldBridge (bridge) where
import HiddenStep (delayed)

{-# INLINE bridge #-}
bridge :: Bool -> [Bool] -> Bool
bridge seed xs = delayed seed xs
HS
"${old[@]}" "${flags[@]}" -this-unit-id "$package" -c LocalFold.hs HiddenStep.hs FoldBridge.hs
ar rcs "$work/libHS$package.a" LocalFold.o HiddenStep.o FoldBridge.o
base=$("$old_pkg" --global-package-db "$old_lib/package.conf.d" field base id --simple-output)
cat > "$work/db/$package.conf" <<EOF
name: r16-late-fold
version: 0.1
id: $package
key: $package
exposed: True
exposed-modules: LocalFold FoldBridge
hidden-modules: HiddenStep
import-dirs: $work/library
library-dirs: $work
hs-libraries: HS$package
depends: $base
EOF
"$old_pkg" --global-package-db "$old_lib/package.conf.d" --package-db "$work/db" recache
flags+=(-package-db "$work/db" -package-id "$package")

check() {
    local pattern=$1 path=$2
    if ! grep -Fq "$pattern" "$path"; then cat "$path" >&2; exit 1; fi
}
no_compile() {
    if grep -Fq 'Compiling ' "$1"; then cat "$1" >&2; exit 1; fi
}
cd "$work/rules-on"
cat > Alias.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, Modifiers #-}
module Alias (Fold) where
import GHC.Exts (U)
import LocalFold (fold)

type Fold :: (a %U -> b %U -> b) -> b -> [a] -> b
type Fold step seed xs = fold step seed xs
HS
cat > Main.hs <<'HS'
{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
module Main where
import Alias (Fold)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import FoldBridge (bridge)

empty :: Proxy step -> Proxy seed -> Fold step seed '[] :~: seed
empty _ _ = Refl

main :: IO ()
main = print (empty Proxy Proxy, bridge False [])
HS
cp Alias.hs Main.hs "$work/rules-off/"
"${old[@]}" "${flags[@]}" -ddump-rule-firings --make Main.hs -o before > old.log 2>&1
check 'Rule fired: late/fold-empty' old.log
test "$(./before)" = '(Refl,True)'
if "${new[@]}" "${flags[@]}" --make Main.hs -o after > upgrade.log 2>&1; then
    cat upgrade.log >&2
    exit 1
fi
check "$reason" upgrade.log
check '[GHC-83605]' upgrade.log
printf '%s\n' 'Old rules-on proof rechecked and rejected.'

cd "$work/rules-off"
off=(-fno-enable-rewrite-rules --make Main.hs -o run)
"${old[@]}" "${flags[@]}" "${off[@]}" > old.log 2>&1
sha256sum ./*.{hi,o} > warm.sha
"${new[@]}" "${flags[@]}" "${off[@]}" > upgrade.log 2>&1
no_compile upgrade.log
sha256sum --quiet -c warm.sha
test "$(./run)" = '(Refl,False)'
printf '%s\n' 'Rules-off interface and result reused.'

cd "$work/ordinary"
cat > Ordinary.hs <<'HS'
module Ordinary (runtime) where
import LocalFold (fold)

runtime :: [Int] -> Int
runtime = fold (+) 0
HS
ordinary=(--make -no-link Ordinary.hs)
"${old[@]}" "${flags[@]}" "${ordinary[@]}" > old.log 2>&1
"${new[@]}" "${flags[@]}" "${ordinary[@]}" > upgrade.log 2>&1
check "$reason" upgrade.log
"${new[@]}" --show-iface Ordinary.hi > ordinary.iface
check "$field" ordinary.iface
sha256sum Ordinary.{hi,o} > warm.sha
"${new[@]}" "${flags[@]}" "${ordinary[@]}" > warm.log 2>&1
no_compile warm.log
sha256sum --quiet -c warm.sha
printf '%s\n' 'Ordinary package use rechecked once and reused on the next build.'
