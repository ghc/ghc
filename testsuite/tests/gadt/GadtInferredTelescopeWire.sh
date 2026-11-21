#!/usr/bin/env bash
set -euo pipefail
ghc=("$1" "-B$2" -v0 -package-env - -i -i. -O0 -dcore-lint -dstg-lint)
out=$(mktemp -d)
trap 'rm -rf -- "$out"' EXIT
cd "$out"

cat >Provider.hs <<'EOF'
{-# LANGUAGE CPP, GADTs, NoExperimentalDependentTypes, RankNTypes #-}
module Provider where

data Early where
    Early :: forall {a} b. a -> b -> Early

#ifdef SPECIFIED
data Specified where
    Specified :: forall {a}. a -> forall b. b -> Specified
#endif

#ifdef INFERRED
data Inferred where
    Inferred :: Int -> forall {a} b. (a, b) -> Inferred
#endif

#ifdef CONTEXT
data Context where
    Context :: Int -> forall {a} b. Show b => (a, b) -> Context
#endif
EOF

cat >Client.hs <<'EOF'
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, TypeAbstractions, TypeApplications #-}
module Main where
import Provider (Inferred(..))

main :: IO ()
main = case Inferred 73 @Bool ('x', True) of
    Inferred n @_ _ -> print n
EOF

previous=()
if [[ -n ${INFERRED_GADT_PREVIOUS_GHC:-} ]]; then
    previous=("$INFERRED_GADT_PREVIOUS_GHC" "-B${INFERRED_GADT_PREVIOUS_LIBDIR:-$2}")
fi
marker=inferred-ordered-gadt-binders

for compression in 0 1 2; do
    for mode in ordinary specified inferred combined; do
        work="$out/$compression/$mode"
        mkdir -p "$work"/{provider,client}
        flags=()
        case "$mode" in
            specified) flags=(-DSPECIFIED) ;;
            inferred) flags=(-DINFERRED) ;;
            combined) flags=(-DINFERRED -DCONTEXT) ;;
        esac

        (
            cd "$work/provider"
            "${ghc[@]}" "${flags[@]}" "-fwrite-if-compression=$compression" \
                -fomit-interface-pragmas -fno-write-if-simplified-core \
                -outputdir . -c "$out/Provider.hs"
            "${ghc[@]}" --show-iface Provider.hi > iface

            case "$mode" in
                ordinary|specified)
                    if grep -Fq "$marker" iface; then
                        echo "Unexpected inferred binder marker in $mode interface"
                        exit 1
                    fi
                    if (( ${#previous[@]} )); then
                        "${previous[@]}" --show-iface Provider.hi > previous.iface
                        cmp iface previous.iface
                    fi
                    ;;
                inferred|combined)
                    suffix="interior-gadt-forall+$marker"
                    if [[ $mode == combined ]]; then
                        suffix="interior-gadt-forall+ordered-gadt-telescope+$marker"
                    fi
                    grep -Fq "retained-features-v1-$suffix" iface
                    if (( ${#previous[@]} )); then
                        # Leave the version available and omit the payload tables.
                        head -c 192 Provider.hi > header.hi
                        for interface in Provider.hi header.hi; do
                            if "${previous[@]}" --show-iface "$interface" > previous.log 2>&1; then
                                echo "Previous reader accepted $mode at compression $compression"
                                exit 1
                            fi
                            grep -Fq 'mismatched interface file versions' previous.log
                            grep -Fq "$marker" previous.log
                        done
                    fi
                    ;;
            esac
        )

        case "$mode" in
            inferred|combined)
                cp "$work/provider/Provider.hi" "$work/client/"
                (
                    cd "$work/client"
                    "${ghc[@]}" -outputdir . -c "$out/Client.hs"
                    "${ghc[@]}" Main.o ../provider/Provider.o -o probe
                    [[ $(./probe) == 73 ]]
                )
                ;;
        esac
    done
done
