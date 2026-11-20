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
if "$ghc" "${flags[@]}" -O1 -fomit-interface-pragmas -fforce-recomp \
    -c provider/Provider.hs -outputdir optimized >optimized.log 2>&1; then
  echo "optimized compilation unexpectedly admitted Pack" >&2
  exit 1
fi
grep -Eq 'Retained -O1 requires checked index domains' optimized.log || {
  cat optimized.log >&2
  exit 1
}
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
cp "$src/"{OrdinaryClient,Eliminate,EliminateMain,QuotePack,TypedQuotePack,ReifyPack,Escape,Scope,WrongWitnessKind,WrongPayload,MissingSchema,WrongSchema,WithoutExtension,RecordMarker,OverSat,OrdinaryPack}.hs .
# The compiler can load the provider only through its interface and object.
"$ghc" "${flags[@]}" -i../provider -c OrdinaryClient.hs
"$ghc" "${flags[@]}" OrdinaryClient.o ../provider/Provider.o -o ordinary
./ordinary
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
"$ghc" "${flags[@]}" EliminateMain.o Eliminate.o ../provider/Provider.o -o eliminate
./eliminate >actual
diff -u - actual <<'EXPECTED'
("present","present","absent","present","present")
("ignored","ignored application","absent")
strict payload pattern forced the package
EXPECTED
cat actual

"$ghc" "${flags[@]}" -i../provider -c OrdinaryPack.hs
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

for bad in Escape Scope WrongWitnessKind WrongPayload MissingSchema WrongSchema WithoutExtension RecordMarker OverSat; do
  if "$ghc" "${flags[@]}" -i../provider -c "$bad.hs" >"$bad.log" 2>&1; then
    echo "$bad unexpectedly compiled" >&2
    exit 1
  fi
  case "$bad" in
    Escape) expected='escape its scope|rigid.*type variable' ;;
    Scope) expected='[Nn]ot in scope' ;;
    WrongWitnessKind|WrongPayload) expected='\[GHC-83865\]' ;;
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
