#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
if [[ $# -ne 4 || $1 != /* || $2 != /* || $3 != /* || $4 != /* ]]; then
    echo "usage: $0 ABS_M169_GHC ABS_M169_LIBDIR ABS_NEW_GHC ABS_NEW_LIBDIR" >&2
    exit 2
fi
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
flags=(-O0 -package-env - -fomit-interface-pragmas -fno-write-if-simplified-core)
old=("$1" "-B$2" "${flags[@]}")
# Hold the package interfaces fixed when comparing compiler recompilation keys.
new=("$3" "-B$2" "${flags[@]}")
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
cp "$src/OpenMapWorker.hs" .
cat >Turn.hs <<'HS'
module Turn where
turn :: Ordering -> Ordering
turn LT = GT
turn EQ = EQ
turn GT = LT
HS
cat >Plain.hs <<'HS'
module Plain where
ident :: a -> a
ident x = x
HS
modules=(OpenMapWorker.hs Turn.hs Plain.hs)
"${old[@]}" -v0 -c "${modules[@]}"
"${new[@]}" --show-iface OpenMapWorker.hi >old.iface
grep -Fq 'T:each ' old.iface
if grep -Fq 'T:signal ' old.iface; then exit 1; fi
cp Plain.hi original.hi
"${new[@]}" --make -no-link "${modules[@]}" >upgrade.log 2>&1
for module in OpenMapWorker Turn; do
    grep -Eq "Compiling $module[[:space:]].*Compiler now checks ordinary enumeration callbacks" upgrade.log
done
cmp original.hi Plain.hi
"${new[@]}" --show-iface OpenMapWorker.hi >worker.iface
"${new[@]}" --show-iface Turn.hi >turn.iface
grep -Fq 'ExecutableSourceTable T:signal' worker.iface
grep -Fq 'ExecutableSourceTable T:turn' turn.iface
"${old[@]}" --show-iface Plain.hi >/dev/null
if "${old[@]}" --show-iface Turn.hi >downgrade.log 2>&1 ||
   ! grep -Fq 'mismatched interface file versions' downgrade.log; then
    cat downgrade.log >&2
    exit 1
fi
"${new[@]}" --make -no-link "${modules[@]}" >warm.log 2>&1
if grep -Fq 'Compiling ' warm.log; then cat warm.log >&2; exit 1; fi

# Installing a rebuilt Prelude can independently change the ABI of an import.
new=("$3" "-B$4" "${flags[@]}")
"${new[@]}" -v0 --make -no-link "${modules[@]}"
"${new[@]}" --show-iface Turn.hi >installed.iface
grep -Fq 'ExecutableSourceTable T:turn' installed.iface
"${new[@]}" --make -no-link "${modules[@]}" >installed-warm.log 2>&1
if grep -Fq 'Compiling ' installed-warm.log; then cat installed-warm.log >&2; exit 1; fi
