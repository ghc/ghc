#!/usr/bin/env bash
set -euo pipefail
if [[ $# -lt 3 || $1 != /* || $2 != /* || $3 != /* ]]; then
    printf 'usage: %s ABS_GHC ABS_LIBDIR ABS_GHC_PKG [GHC_OPTION ...]\n' "$0" >&2
    exit 2
fi
libdir=$2
ghc_pkg=$3
ghc=("$1" "-B$libdir" -O0 -v0 -package-env - -i -dcore-lint -dstg-lint -fdiagnostics-color=never)
shift 3
ghc+=("$@")
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
mkdir "$out"/{db,interfaces,exporter,client,staged}
source "$src/NativeOpenInstalledInterfaces.sh" "$libdir" "$ghc_pkg"

# The package view must contain types but no compiled code for the tuple.
"${ghc[@]}" --show-iface "$out/interfaces/GHC/Internal/Data/Tuple.hi" >"$out/tuple.iface"
if grep -q '^extra decls:' "$out/tuple.iface"; then
    printf 'Unexpected embedded executable Core for the tuple module\n' >&2
    exit 1
fi

expect_rejection() {
    local file=$1 pattern=$2 diagnostic=$3 source_text=$4
    local line
    line=$(awk -v pattern="$pattern" '$0 ~ pattern { print NR; exit }' "$file")
    if "${ghc[@]}" "${db[@]}" -i"$out/interfaces" -c "$file" >reject.log 2>&1 ||
       ! grep -Eq "^${file%.hs}\\.hs:$line:[0-9]+(-[0-9]+)?: error:" reject.log ||
       ! grep -Eq "$diagnostic" reject.log ||
       ! grep -Fq "$source_text" reject.log; then
        cat reject.log >&2
        return 1
    fi
}

cd "$out/exporter"
cp "$src"/{PreludePairExport,WrongRigidPair}.hs .
"${ghc[@]}" "${db[@]}" -fomit-interface-pragmas -c PreludePairExport.hs
"${ghc[@]}" --show-iface PreludePairExport.hi >exported.iface
grep -Fq 'GHC.Internal.Data.Tuple.T:fst' exported.iface
grep -Fq 'GHC.Internal.Data.Tuple.T:snd' exported.iface
cp PreludePairExport.hi "$out/interfaces/"
expect_rejection WrongRigidPair.hs '^wrong _ _ = Refl' '\[GHC-25897\]' 'x :~: fst p'

cd "$out/client"
cp "$src"/{NativeOpenInstalledPair,WrongGroundPair,WrongPairNamespace}.hs .
"${ghc[@]}" "${db[@]}" -i"$out/interfaces" -c NativeOpenInstalledPair.hs
expect_rejection WrongGroundPair.hs '^wrong = first' '\[GHC-(83865|18872)\]' 'False'
expect_rejection WrongPairNamespace.hs '^wrong ::' '\[GHC-37479\]' 'is a term-level binding'
# Only the link uses the ordinary library registrations and exporter object.
"${ghc[@]}" NativeOpenInstalledPair.o ../exporter/PreludePairExport.o -o client
./client

# With the normal package database, preserve old R15 staged calls even if
# the source module later adds matching specialization rules.
cd "$out/staged"
cp "$src/InstalledStagedPair.hs" .
"${ghc[@]}" -fno-omit-interface-pragmas -fenable-rewrite-rules -c InstalledStagedPair.hs
"${ghc[@]}" --show-iface InstalledStagedPair.hi >staged.iface
grep -Fq 'installed/fst/natural' staged.iface
grep -Fq 'installed/snd/natural' staged.iface
