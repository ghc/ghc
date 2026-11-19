#!/usr/bin/env bash
set -euo pipefail

src=$(cd -- "$(dirname -- "$0")" && pwd)
ghc=("$1" "-B$2" -v0 -package-env - -dcore-lint -dstg-lint)
tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT
cd "$tmp"
mkdir provider client
cp "$src/T14038.hs" provider/
(cd provider && "${ghc[@]}" -c T14038.hs)
cp provider/T14038.hi "$src"/NativeOpenMatchability{Client,Conflict}.hs client/
cd client
"${ghc[@]}" -c NativeOpenMatchabilityClient.hs
if "${ghc[@]}" -c NativeOpenMatchabilityConflict.hs >conflict.log 2>&1 ||
    ! grep -Fq 'GHC-83865' conflict.log ||
    ! grep -Fq 'Maybe' conflict.log ||
    ! grep -Fq 'Unmatchable' conflict.log; then
    cat conflict.log >&2
    exit 1
fi
