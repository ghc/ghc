#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

src=$(cd "$(dirname "$0")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -dcore-lint -dstg-lint
     -fdiagnostics-color=never)

for opt in O0 O1; do
  mkdir -p "$out/$opt"/{provider,client}
  cd "$out/$opt/provider"
  for side in Left Right; do
    cp "$src/NativeScopedSumMatch$side.hs" .
    "${ghc[@]}" "-$opt" -c "NativeScopedSumMatch$side.hs"
  done
  cp ./*.hi "$src"/NativeScopedSumMatch{Client,Reject}.hs ../client/
  cd ../client
  "${ghc[@]}" "-$opt" -c NativeScopedSumMatchClient.hs

  status=0
  "${ghc[@]}" "-$opt" -c NativeScopedSumMatchReject.hs >reject.log 2>&1 || status=$?
  if [[ $status != 1 || $(grep -Fc '[GHC-83865]' reject.log) != 5 ]] ||
     ! grep -Eq 'match type .False. with .True.' reject.log; then
    cat reject.log
    exit 1
  fi
  for binding in wrongFallback namedMatch namedFallback foreignMatch foreignFallback; do
    if ! grep -Eq "In an equation for .$binding." reject.log; then
      cat reject.log
      exit 1
    fi
  done
done
