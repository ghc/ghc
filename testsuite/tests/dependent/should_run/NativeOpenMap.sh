#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc=("$1" "-B$2" -O0 -v0 -package-env - -fomit-interface-pragmas
     -fno-write-if-simplified-core -dcore-lint -dstg-lint)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cp "$src"/{OpenMapWorker,OpenMapSource,OpenMapProof}.hs "$out/"
cd "$out"
cp OpenMapWorker.hs Worker.source

compile_worker() {
    "${ghc[@]}" -c OpenMapWorker.hs
    "${ghc[@]}" --show-iface OpenMapWorker.hi >worker.iface
    rm OpenMapWorker.hs OpenMapWorker.o
}
compile_worker
grep -Fq 'T:each ' worker.iface
"${ghc[@]}" -c OpenMapSource.hs
rm OpenMapSource.hs OpenMapSource.o
"${ghc[@]}" -c OpenMapProof.hs

reject_worker() {
    compile_worker
    if grep -Fq 'T:each ' worker.iface; then
        echo 'The changed mapper still published an equation.' >&2
        exit 1
    fi
}

# Replace the carried callback, repeat the scrutinee, or force the callback.
sed 's/each f xs/each (const (f x)) xs/' Worker.source >OpenMapWorker.hs
reject_worker
sed 's/each f xs/each f (x:xs)/' Worker.source >OpenMapWorker.hs
reject_worker
sed 's/= f x : each f xs/= f `seq` (f x : each f xs)/' Worker.source >OpenMapWorker.hs
reject_worker
