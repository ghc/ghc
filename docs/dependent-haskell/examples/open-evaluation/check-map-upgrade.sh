#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C
if [[ $# -ne 3 || $1 != /* || $2 != /* || $3 != /* ]]; then
    echo "usage: $0 ABS_OLD_GHC ABS_OLD_LIBDIR ABS_NEW_GHC" >&2
    exit 2
fi
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../../../.." && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"
module=NativeOpenInstalledMapProducer
cp "$root/testsuite/tests/dependent/should_run/$module.hs" .

# Hold the package interfaces fixed while the compiler changes.
old=("$1" "-B$2")
new=("$3" "-B$2")
flags=(-O1 -package-env - -dcore-lint -dstg-lint --make -no-link "$module.hs")
"${old[@]}" -v0 "${flags[@]}"
"${new[@]}" --show-iface "$module.hi" >before.iface
if grep -Fq 'FinalCoreSourceTable' before.iface; then
    echo 'Use a compiler from before M173 for the first build.' >&2
    exit 1
fi
"${new[@]}" "${flags[@]}" >upgrade.log 2>&1
grep -F 'Compiler now validates optimized source mappers' upgrade.log
"${new[@]}" --show-iface "$module.hi" >after.iface
grep -F 'FinalCoreSourceTable T:mapper' after.iface
"${new[@]}" "${flags[@]}" >warm.log 2>&1
if grep -Fq 'Compiling ' warm.log; then cat warm.log >&2; exit 1; fi
if "${old[@]}" --show-iface "$module.hi" >downgrade.log 2>&1 ||
   ! grep -Fq 'final-core-mapper-v1' downgrade.log; then
    cat downgrade.log >&2
    exit 1
fi
