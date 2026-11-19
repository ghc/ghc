#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never)
shift 2
ghc+=("$@")
src=$PWD
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{exporter,client,producer}

# An optimized module can export a type alias without using an equation.
cd "$out/exporter"
cp "$src/PreludeMapSource.hs" .
"${ghc[@]}" -O1 -fenable-rewrite-rules -c PreludeMapSource.hs
cd "$out/client"
cp ../exporter/PreludeMapSource.hi "$src/PreludeMapProof.hs" .

# Only the alias interface is present, and the client has no dependent syntax.
# Use explicit flags to test policy independently of the optimization level.
"${ghc[@]}" -O1 -fno-enable-rewrite-rules -c PreludeMapProof.hs
line=$(awk '/^preludeNil =/ { print NR }' PreludeMapProof.hs)
if "${ghc[@]}" -O0 -fenable-rewrite-rules -fforce-recomp -c PreludeMapProof.hs >rules.log 2>&1 ||
   ! grep -Fq "PreludeMapProof.hs:$line:" rules.log ||
   ! grep -Fq '[GHC-83605]' rules.log ||
   ! grep -Fq 'only when rewrite rules are disabled' rules.log ||
   ! grep -Fq 'Use -fno-enable-rewrite-rules in the module that needs this equation.' rules.log; then
    cat rules.log >&2
    exit 1
fi

# A no-code build must not issue final-Core provenance. An ordinary producer
# can publish the certificate, or withdraw it and compile after a real rule.
cd "$out/producer"
cp "$src/NativeOpenInstalledMapProducer.hs" .
producer=("${ghc[@]}" -O1 -fforce-recomp -c NativeOpenInstalledMapProducer.hs)
interface=NativeOpenInstalledMapProducer.hi
"${producer[@]}" -fno-code -fwrite-interface
"${ghc[@]}" --show-iface "$interface" >producer.iface
if grep -Eq 'FinalCoreSourceTable|T:mapper' producer.iface; then
    exit 1
fi
"${producer[@]}"
"${ghc[@]}" --show-iface "$interface" >producer.iface
grep -Fq 'FinalCoreSourceTable T:mapper' producer.iface
"${producer[@]}" -DCHANGE_MAPPER -ddump-rule-firings >producer.rules
grep -Fq 'Rule fired: drop mapper traversal' producer.rules
"${ghc[@]}" --show-iface "$interface" >producer.iface
if grep -Eq 'FinalCoreSourceTable|T:mapper' producer.iface; then
    exit 1
fi
