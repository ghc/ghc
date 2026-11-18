#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

ghc=("$1" "-B$2" -v0 -O1 -fno-ignore-interface-pragmas -fenable-rewrite-rules
     -fno-write-if-simplified-core -fdiagnostics-color=never)
shift 2
ghc+=("$@")
client=NativeClosedSumDependencies
instance=${client}Instance
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cp -- "$src"/NativeClosedSumDependencies*.hs "$out/"
cd "$out"
build=("${ghc[@]}" --make -no-link)
if "${ghc[@]}" --info | grep -F '"Support dynamic-too","YES"' >/dev/null; then
    build+=(-dynamic-too)
fi

"${build[@]}" "$client.hs" -ddump-rule-firings > rules.log 2>&1 ||
    { cat rules.log >&2; exit 1; }
grep -Fq 'Class op pick' rules.log || { cat rules.log >&2; exit 1; }
"${ghc[@]}" --show-iface "$client.hi" > client.iface

# Require the selected dictionary in the instance module's import usage.
# The selector rule removed the instance object from the runtime usage.
if ! awk -v mod="main:$instance" '
    /^[[:space:]]*usages: / { in_usage = 1 }
    /^[[:space:]]*orphan hash:/ { in_usage = 0 }
    in_usage && /import / { in_instance = index($0, mod " ") != 0 }
    in_usage && in_instance && $1 == "$fPickNatural" {
        hash = $2
        gsub(/,|]/, "", hash)
        if (length(hash) == 32 && hash !~ /[^0-9a-f]/) found = 1
    }
    END { exit !found }
' client.iface ||
    ! grep -Eq "^ *addDependentFile .*${client}Helper\\.(dyn_)?o\"" client.iface ||
    grep -Eq "^ *addDependentFile .*$instance\\.(dyn_)?o\"" client.iface; then
    cat client.iface >&2
    exit 1
fi

"${build[@]}" "$client.hs" -v2 > unchanged.log 2>&1
grep -Eq "Skipping[[:space:]]+$client([[:space:]]|$)" unchanged.log ||
    { cat unchanged.log >&2; exit 1; }

# Change only the instance. Both methods already exist in the helper module.
sed 's/first/second/g' "$instance.hs" > changed.hs
mv changed.hs "$instance.hs"
if "${build[@]}" "$client.hs" > changed.log 2>&1; then
    printf '%s\n' 'The unchanged client accepted the stale result.' >&2
    exit 1
fi
line=$(awk '/^answer = Ten$/ { print NR }' "$client.hs")
if ! grep -Eq "^$client\\.hs:$line:[0-9]+(-[0-9]+)?: error:" changed.log ||
   ! grep -Fq 'Expected: NatTag 11' changed.log ||
   ! grep -Fq 'Actual: NatTag 10' changed.log; then
    cat changed.log >&2
    exit 1
fi
