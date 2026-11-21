#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

if [[ $# != 4 || $1 != /* || $2 != /* || $3 != /* || $4 != /* ]]; then
    printf 'usage: %s ABS_GHC ABS_LIBDIR ABS_GHC_PKG ABS_NEW_OUTPUT_DIR\n' "$0" >&2
    exit 2
fi

src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
out=$4
ghc=("$1" "-B$2" -v0 -package-env - -clear-package-db -global-package-db
     -dcore-lint -dstg-lint)
ghc_pkg=("$3" "--global-package-db=$2/package.conf.d" --no-user-package-db)
unit=boundary-candidate-0.1
pkg=$out/plugin
db=$out/plugin.conf.d

mkdir "$out"
mkdir "$pkg" "$out/oracle" "$out/frontend"
cp -- "$src"/{BoundaryEvidence,LocatePlugin}.hs "$pkg/"
(
    cd "$pkg"
    cat >LatePlugin.hs <<'HS'
module LatePlugin (plugin) where

import GHC.Plugins (CoreToDo (CoreDoPluginPass), Plugin (..), defaultPlugin)

plugin :: Plugin
plugin = defaultPlugin
    { installCoreToDos = \_ todos ->
        pure (CoreDoPluginPass "Late control" (\_ -> error "late pass ran") : todos)
    }
HS
    "${ghc[@]}" --make -dynamic -hisuf dyn_hi -osuf dyn_o -fPIC -shared \
        -this-unit-id "$unit" -package ghc \
        LocatePlugin.hs LatePlugin.hs -o "libHS$unit-ghc$("${ghc[@]}" --numeric-version).so"
)
"${ghc_pkg[@]}" init "$db"
deps=$("${ghc_pkg[@]}" field base id --simple-output)
deps+=" $("${ghc_pkg[@]}" field ghc id --simple-output)"
cat >"$pkg/package.conf" <<EOF
name: boundary-candidate
version: 0.1
id: $unit
key: $unit
exposed: True
exposed-modules: BoundaryEvidence LocatePlugin LatePlugin
import-dirs: \${pkgroot}/plugin
library-dirs: \${pkgroot}/plugin
dynamic-library-dirs: \${pkgroot}/plugin
hs-libraries: HS$unit
depends: $deps
EOF
"${ghc_pkg[@]}" --package-db="$db" register "$pkg/package.conf"
plugin_db=(-package-db "$db" -package-id "$unit")
installed=("${plugin_db[@]}" -fplugin=LocatePlugin)

# Evaluating an ordinary function in a type needs its executable code.
cp -- "$src/OrdinaryOffer.hs" "$out/oracle/"
(
    cd "$out/oracle"
    "${ghc[@]}" -dynamic -c OrdinaryOffer.hs
)

expect_failure() {
    local log=$1 expected=$2
    shift 2
    if "$@" >"$log" 2>&1 || ! grep -Fq "$expected" "$log"; then
        cat "$log" >&2
        exit 1
    fi
}

(
    cd "$out/frontend"
    cat >Probe.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module Probe where
import BoundaryEvidence (Member, location)
import qualified OrdinaryOffer

request :: Member 8 (OrdinaryOffer.offered 7)
request = location
HS
    frontend=("${ghc[@]}" -c -dynamic -fno-code -fno-write-interface -fforce-recomp -i "-i$out/oracle")
    expect_failure unaided.log 'Locate 8' \
        "${frontend[@]}" -package-db "$db" -package-id "$unit" Probe.hs
    "${frontend[@]}" "${installed[@]}" "-fplugin-opt=LocatePlugin:$PWD/right.position" Probe.hs
    grep -Fxq 1 right.position

    # Zero is a valid path at input eight. The native proof still uses seven.
    sed -e 's/module Probe/module Other/' -e 's/offered 7/offered 8/' Probe.hs >Other.hs
    "${frontend[@]}" "${installed[@]}" "-fplugin-opt=LocatePlugin:$PWD/wrong.position" Other.hs
    grep -Fxq 0 wrong.position
    [[ -z $(find . \( -name '*.hi' -o -name '*.o' \) -print -quit) ]]
)

for opt in 0 2; do
    native=$out/native-O$opt
    mkdir -p "$native"/{objects,interfaces,consumer}
    cp -- "$src"/{BoundaryEvidence,OrdinaryOffer,EmitMembership,NativeOffer}.hs "$native/"
    cp -- "$src/UseNativeOffer.hs" "$native/consumer/"
    (
        cd "$native"
        cp -- "$out/frontend/right.position" offer.position
        provider=("${ghc[@]}" "-O$opt" --make -no-link -outputdir objects
                  -fomit-interface-pragmas -fno-write-if-simplified-core)
        "${provider[@]}" NativeOffer.hs
        cp -- objects/NativeOffer.hi interfaces/
        "${ghc[@]}" --show-iface interfaces/NativeOffer.hi >interface.txt
        grep -Eq 'addDependentFile "[^"]*/offer[.]position"' interface.txt
        (
            cd consumer
            "${ghc[@]}" "-O$opt" -i -i../interfaces -c UseNativeOffer.hs
            "${ghc[@]}" UseNativeOffer.o ../objects/{NativeOffer,OrdinaryOffer,BoundaryEvidence}.o -o use-offer
            ./use-offer | tee runtime.txt
            grep -Fxq '("eight",True)' runtime.txt
        )

        cp -- "$out/frontend/wrong.position" offer.position
        expect_failure wrong.log "Couldn't match type" "${provider[@]}" NativeOffer.hs
        grep -Fq Here wrong.log

        cp -- "$out/frontend/right.position" offer.position
        "${provider[@]}" NativeOffer.hs
        for plugin in LocatePlugin LatePlugin; do
            mkdir "$plugin"
            cp -- ./{BoundaryEvidence,OrdinaryOffer,EmitMembership}.hs offer.position "$plugin/"
            {
                # Keep the plugin flag off the ordinary dependencies.
                printf '{-# OPTIONS_GHC -fplugin=%s #-}\n' "$plugin"
                cat NativeOffer.hs
            } >"$plugin/NativeOffer.hs"
            (
                cd "$plugin"
                expect_failure native.log 'Native dependent Core cannot enter module plugins' \
                    "${provider[@]}" -dynamic "${plugin_db[@]}" NativeOffer.hs
                if grep -Fq 'late pass ran' native.log; then
                    cat native.log >&2
                    exit 1
                fi
            )
        done
    )
done
