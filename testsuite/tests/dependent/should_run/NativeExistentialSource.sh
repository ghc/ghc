#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
ghc=$1
lib=$2
shift 2
src=$(cd -- "$(dirname -- "$0")/erased-existential-source" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
flags=(-B"$lib" -v0 -package-env - -O0 -dcore-lint -dstg-lint
       -fomit-interface-pragmas -fno-write-if-simplified-core
       -fdiagnostics-color=never "$@")

mkdir provider client
cp "$src/Provider.hs" provider/
"$ghc" "${flags[@]}" -ddump-ds -ddump-stg-final -c provider/Provider.hs \
  -outputdir provider -fwrite-ide-info -hiedir provider >provider/core-stg.log
test -s provider/Provider.hie
mv provider/Provider.hs hidden-provider-source
grep -Eq '\bpack\b' provider/core-stg.log || { cat provider/core-stg.log; exit 1; }
sed -n '/Final STG:/,$p' provider/core-stg.log >provider/stg.log
test -s provider/stg.log
if grep -Eiq '\b(Pack|Unpack|PackTag)\b' provider/stg.log; then
  echo "existential constructor escaped into provider STG" >&2
  exit 1
fi
echo "built provider with Core Pack, erased STG and HIE"
cd client
cp "$src/"{OrdinaryClient,Eliminate,EliminateMain,QuotePack,TypedQuotePack,ReifyPack,Escape,Scope,WrongWitnessKind,WitnessTerm,WrongPayload,MissingSchema,WrongSchema,WithoutExtension,RecordMarker,OverSat,OrdinaryPack}.hs .
# The compiler can load the provider only through its interface and object.
"$ghc" "${flags[@]}" -i../provider -c OrdinaryClient.hs
"$ghc" "${flags[@]}" OrdinaryClient.o ../provider/Provider.o -o ordinary
./ordinary | tee ordinary.stdout
"$ghc" "${flags[@]}" -c OrdinaryPack.hs
"$ghc" "${flags[@]}" -i../provider -c Eliminate.hs -fwrite-ide-info \
  -ddump-hie -ddump-ds -ddump-stg-final >eliminate.log
test -s Eliminate.hie
grep -Eq '\bunpack\b' eliminate.log || { cat eliminate.log; exit 1; }
sed -n '/Final STG:/,$p' eliminate.log >eliminate.stg
test -s eliminate.stg
if grep -Eiq '\b(Pack|Unpack|PackTag)\b' eliminate.stg; then
  echo "existential expression escaped into elimination STG" >&2
  exit 1
fi
"$ghc" "${flags[@]}" -i../provider -c EliminateMain.hs
"$ghc" "${flags[@]}" EliminateMain.o Eliminate.o OrdinaryPack.o ../provider/Provider.o -o eliminate
./eliminate >actual
diff -u - actual <<'EXPECTED'
("present","present","absent","present","present")
("ignored","ignored application","absent")
13
("[True]","[7]")
strict payload pattern forced the package
EXPECTED
cat actual

# Compile the producer and both clients with the normal optimized interfaces.
mkdir ../optimized
(
    cd ../optimized
    opt=(-B"$lib" -v0 -O1 -package-env - -dcore-lint -dstg-lint "$@")
    cp "$src"/{Provider,OrdinaryClient,OrdinaryPack,Eliminate,EliminateMain}.hs .
    "$ghc" "${opt[@]}" -drule-check probe -c Provider.hs >rule-check.log 2>&1
    grep -Fq "Rule check" rule-check.log
    mv Provider.hs Provider.source
    "$ghc" "${opt[@]}" -c OrdinaryClient.hs
    "$ghc" "${opt[@]}" OrdinaryClient.o Provider.o -o ordinary
    ./ordinary | cmp - ../client/ordinary.stdout
    "$ghc" "${opt[@]}" -c OrdinaryPack.hs
    "$ghc" "${opt[@]}" -c Eliminate.hs
    mv Eliminate.hs Eliminate.source
    "$ghc" "${opt[@]}" -c EliminateMain.hs
    "$ghc" "${opt[@]}" EliminateMain.o Eliminate.o OrdinaryPack.o Provider.o -o eliminate
    ./eliminate | cmp - ../client/actual
)
echo "optimized package providers and clients preserve payload demand"

# A module-level rule still requires the restricted optimization setting.
cat >RuleOwner.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, ScopedTypeVariables #-}
module RuleOwner where
import Data.Kind (Type)
type Hidden = exists (a :: Type). Maybe a
packValue :: Bool -> Hidden
packValue x = Just x
{-# RULES "package/id" forall (x :: Bool). id x = x #-}
HS
if "$ghc" "${flags[@]}" -O1 -Wno-inline-rule-shadowing -c RuleOwner.hs >rule.log 2>&1; then
    echo "unexpectedly admitted a dependent module's rewrite rule" >&2
    exit 1
fi
grep -Fq '[GHC-51063]' rule.log && grep -Fq 'package/id' rule.log || {
    cat rule.log >&2
    exit 1
}

mkdir -p fake/GHC
cp "$src/fake/GHC.Exists.hs" fake/GHC/Exists.hs
cp "$src/fake/Fake.hs" fake/Fake.hs
"$ghc" "${flags[@]}" -c fake/GHC/Exists.hs -outputdir fake
"$ghc" "${flags[@]}" -ifake -c fake/Fake.hs -outputdir fake
echo "ordinary namesakes in a different module and a different unit compile"

"$ghc" "${flags[@]}" -i../provider -c QuotePack.hs
"$ghc" "${flags[@]}" -i../provider -c TypedQuotePack.hs
"$ghc" "${flags[@]}" -i../provider -c ReifyPack.hs
echo "typed and untyped quotations and guarded Pack reification pass"

for bad in Escape Scope WrongWitnessKind WitnessTerm WrongPayload MissingSchema WrongSchema WithoutExtension RecordMarker OverSat; do
  if "$ghc" "${flags[@]}" -i../provider -c "$bad.hs" >"$bad.log" 2>&1; then
    echo "$bad unexpectedly compiled" >&2
    exit 1
  fi
  case "$bad" in
    Escape)
      expected='escape its scope|rigid.*type variable'
      grep -Fq 'badWildcard' "$bad.log" || { cat "$bad.log" >&2; exit 1; }
      ;;
    Scope) expected='[Nn]ot in scope' ;;
    WrongWitnessKind|WrongPayload) expected='\[GHC-83865\]' ;;
    WitnessTerm) expected='Illegal term-level use of the type variable' ;;
    MissingSchema) expected='needs an explicit existential type when' ;;
    WrongSchema) expected='The available type is' ;;
    WithoutExtension) expected='requires -XExperimentalDependentTypes' ;;
    RecordMarker) expected='explicit existential application or a sole case alternative' ;;
    OverSat) expected='Apply GHC.Exists.Pack to a required type witness and a payload' ;;
  esac
  if ! grep -Eq "$bad[.]hs:[0-9]+:[0-9]+: error" "$bad.log" ||
     ! grep -Eq "$expected" "$bad.log" ||
     grep -Eiq 'panic|internal error' "$bad.log"; then
    cat "$bad.log" >&2
    exit 1
  fi
done
