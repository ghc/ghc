#!/usr/bin/env bash
set -euo pipefail
export LC_ALL=C

# Run with two matching native toolchains. The old compiler predates the
# readback recipes and the matchability inference. The optional build directory
# supplies the old boot interfaces for the original GHC.Internal.Err source.
upgrade=
case ${1:-} in
    --family-kinds|--family-results|--packed-projections) upgrade=$1; shift ;;
esac
if (( $# < 4 || $# > 5 )) || { [[ $upgrade == --family-* ]] && (( $# != 4 )); }; then
    echo "Usage: $0 OLD_GHC OLD_LIB NEW_GHC NEW_LIB [OLD_GHC_INTERNAL_BUILD]" >&2
    echo "       $0 --family-kinds M177_GHC M177_LIB NEW_GHC NEW_LIB" >&2
    echo "       $0 --family-results INTERMEDIATE_GHC INTERMEDIATE_LIB NEW_GHC NEW_LIB" >&2
    echo "       $0 --packed-projections OLD_GHC OLD_LIB NEW_GHC NEW_LIB [NEW_GHC_PKG]" >&2
    exit 2
fi
src=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo=${GHC_SOURCE_ROOT:-$(cd "$src/../../../.." && pwd)}
old_ghc=$(realpath "$1")
old_lib=$(realpath "$2")
new_ghc=$(realpath "$3")
new_lib=$(realpath "$4")
boot_dir=
ghc_pkg=
if (( $# == 5 )); then
    if [[ $upgrade == --packed-projections ]]; then ghc_pkg=$(realpath "$5")
    else boot_dir=$(realpath "$5")
    fi
fi
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
mkdir "$work"/{home,binary,err,bare}
lib="$new_lib"
if [[ -n $boot_dir ]]; then
    cp -al "$new_lib" "$work/lib"
    lib="$work/lib"
fi
old=("$old_ghc" "-B$old_lib" -package-env -)
new=("$new_ghc" "-B$lib" -package-env - -fexternal-interpreter)
sanity=(-dcore-lint -dstg-lint)
new_companions='readback2ZC(scalarzmtable|[0-9]+ZC)'

replace() {
    cp "$1" "$2.new"
    mv -f "$2.new" "$2"
}

if [[ $upgrade == --packed-projections ]]; then
    packed="$repo/docs/dependent-haskell/examples/packed-vector"
    check_projection() (
        # In the installed variant keep the producer package off the source path.
        cd "$1"
        shift
        args=(-v1 -O0 -fomit-interface-pragmas "${sanity[@]}" "$@")
        cp "$packed"/{PackedLength,PackedProjection}.hs .
        cat >Unrelated.hs <<'HS'
module Unrelated where
import PackedVecLibrary (Nat(..))
answer :: Int -> Nat
answer _ = Z
HS
        roots=(PackedLength.hs Unrelated.hs)
        untouched=(Unrelated.hi Unrelated.o)
        if (( $# == 0 )); then
            cp "$packed"/PackedVecLibrary.hs .
            cat >AbstractUser.hs <<'HS'
module AbstractUser where
import PackedVecLibrary (Nat(..), PackedVec)
ignore :: PackedVec a -> Nat
ignore _ = Z
HS
            roots+=(AbstractUser.hs)
            untouched+=({PackedVecLibrary,AbstractUser}.{hi,o})
        fi
        "${old[@]}" "${args[@]}" --make -no-link "${roots[@]}"
        "${old[@]}" "${args[@]}" --show-iface PackedLength.hi >worker.old
        if grep -F 'T:packageLength' worker.old; then
            echo "The old worker already has a projection equation." >&2
            exit 1
        fi
        mkdir stale
        cp PackedLength.hi PackedProjection.hs stale/
        if [[ -f PackedVecLibrary.hi ]]; then cp PackedVecLibrary.hi stale/; fi
        if (cd stale && "${new[@]}" "${args[@]}" -c PackedProjection.hs) >stale.log 2>&1 ||
           ! grep -Fq 'No checked equations are available' stale.log; then cat stale.log >&2; exit 1; fi

        sha256sum "${untouched[@]}" >untouched.sha
        roots+=(PackedProjection.hs)
        "${new[@]}" "${args[@]}" --make -no-link "${roots[@]}" >upgrade.log 2>&1 ||
            { cat upgrade.log >&2; exit 1; }
        grep -F 'Compiling PackedLength [Compiler now checks retained package case projections]' upgrade.log
        if grep -E 'Compiling (PackedVecLibrary|AbstractUser|Unrelated)( |$)' upgrade.log; then exit 1; fi
        sha256sum -c untouched.sha --quiet
        "${new[@]}" "${args[@]}" --show-iface PackedLength.hi >worker.new
        grep -Fq 'T:packageLength' worker.new
        sha256sum ./*.hi ./*.o >warm.sha
        "${new[@]}" "${args[@]}" --make -no-link "${roots[@]}" >warm.log 2>&1 ||
            { cat warm.log >&2; exit 1; }
        if grep -F 'Compiling ' warm.log; then exit 1; fi
        sha256sum -c warm.sha --quiet

        sed 's/packageLength (PackVec n _) = n/packageLength (PackVec n _) = S n/' PackedLength.hs >PackedLength.new
        mv PackedLength.new PackedLength.hs
        if "${new[@]}" "${args[@]}" --make -no-link "${roots[@]}" >changed.log 2>&1 ||
           ! grep -Fq 'Compiling PackedProjection [PackedLength changed]' changed.log ||
           ! grep -Fq '[GHC-05617]' changed.log; then cat changed.log >&2; exit 1; fi
    )

    check_projection "$work/home"
    if [[ -n $ghc_pkg ]]; then
        mkdir "$work/provider"
        (
            cd "$work/provider"
            unit=retained-upgrade-0.1
            pkg=("$ghc_pkg" --global-package-db "$old_lib/package.conf.d")
            base=$("${pkg[@]}" field base id --simple-output)
            "${pkg[@]}" init "$work/provider-db"
            cp "$packed/PackedVecLibrary.hs" .
            "${old[@]}" "${sanity[@]}" -O0 -fomit-interface-pragmas -this-unit-id "$unit" -c PackedVecLibrary.hs
            cat >provider.conf <<CONF
name: retained-upgrade
version: 0.1
id: $unit
key: $unit
exposed: True
exposed-modules: PackedVecLibrary
import-dirs: $PWD
depends: $base
CONF
            "${pkg[@]}" --package-db "$work/provider-db" register provider.conf
            rm PackedVecLibrary.hs
            sha256sum PackedVecLibrary.{hi,o} >provider.sha
        )
        check_projection "$work/binary" -package-db "$work/provider-db" -package-id retained-upgrade-0.1
        (cd "$work/provider" && sha256sum -c provider.sha --quiet)
    fi
    echo 'Packed projection upgraded, warm artifacts were reused, and the stale and changed proofs were rejected.'
    exit 0
fi

if [[ $upgrade == --family-* ]]; then
    cd "$work/home"
    cat >IndependentIndices.hs <<'HS'
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
module IndependentIndices where
import Data.Kind (Type)
type family Rep (b :: Bool) :: Type
type instance Rep 'True = Type -> Type
type family Arrow (f :: Rep 'True) :: Type
type family Use (f :: Rep 'True) (g :: Arrow f) :: Type
type instance Arrow Maybe = Type -> Type
identity :: Arrow Maybe
identity x = x
HS
    cat >Unrelated.hs <<'HS'
module Unrelated where
answer :: Int
answer = 42
HS
fi

if [[ $upgrade == --family-kinds ]]; then
    cat >FamilyKinds.hs <<'HS'
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
module FamilyKinds (PFrom) where
import Data.Kind (Type)
type family Rep (k :: Type) :: Type
type instance Rep () = ()
type family PFrom (a :: k) :: Rep k
HS
    cat >FamilyKindsClient.hs <<'HS'
{-# LANGUAGE DataKinds, TypeFamilies #-}
module FamilyKindsClient where
import Data.Proxy (Proxy(..))
import FamilyKinds (PFrom)
observation :: Proxy (PFrom '())
observation = Proxy
HS
    cat >>IndependentIndices.hs <<'HS'
type instance Use Maybe Maybe = Bool
observation :: Use Maybe Maybe
observation = True
HS
    args=(--make -no-link -v1 "${sanity[@]}" FamilyKindsClient.hs IndependentIndices.hs Unrelated.hs)
    "${old[@]}" "${args[@]}"
    sha256sum Unrelated.{hi,o} >unrelated.sha
    "${new[@]}" "${args[@]}" >upgrade.log 2>&1 || { cat upgrade.log >&2; exit 1; }
    grep -F 'Compiling FamilyKinds [Compiler now infers matchability in family kinds]' upgrade.log
    grep -F 'Compiling FamilyKindsClient [FamilyKinds changed]' upgrade.log
    grep -F 'Compiling IndependentIndices [Compiler now infers matchability in family kinds]' upgrade.log
    sha256sum -c unrelated.sha --quiet
    if grep -F 'Compiling Unrelated' upgrade.log; then exit 1; fi
    "${new[@]}" "${args[@]}" >warm.log 2>&1
    if grep -F 'Compiling ' warm.log; then exit 1; fi
    exit 0
fi

if [[ $upgrade == --family-results ]]; then
    cat >TwoFamilyResults.hs <<'HS'
{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies #-}
module TwoFamilyResults where
import Data.Kind (Type)
type family Rep (b :: Bool) :: Type
type instance Rep 'True = Type -> Type
type instance Rep 'False = Type -> Type
type family Arrow (f :: Rep 'True) (g :: Rep 'False) :: Type
type instance Arrow Maybe Maybe = Type -> Type
type family Use (f :: Rep 'True) (g :: Rep 'False) (h :: Arrow f g) :: Type
HS
    cat >ExplicitKind.hs <<'HS'
{-# LANGUAGE DataKinds, Modifiers, PolyKinds, StandaloneKindSignatures, TypeFamilies #-}
module ExplicitKind where
import Data.Kind (Type)
import GHC.Exts (Matchability)
type Fixed :: forall {m :: Matchability}. (Type %m -> Type) -> Type
type family Fixed f
type instance Fixed Maybe = Bool
type family Receiving (b :: Fixed Maybe) :: Type
type instance Receiving 'True = Int
HS
    args=(--make -no-link -v1 "${sanity[@]}" ExplicitKind.hs Unrelated.hs)
    "${old[@]}" "${args[@]}" IndependentIndices.hs TwoFamilyResults.hs
    "${old[@]}" --show-iface ExplicitKind.hi >explicit.old
    grep -F '  ABI hash:' explicit.old >explicit.old.abi
    sha256sum Unrelated.{hi,o} >unrelated.sha
    stat -c '%y %n' Unrelated.{hi,o} >unrelated.time

    # Add the clients after the intermediate compiler writes the owner interfaces.
    cat >IndependentIndicesClient.hs <<'HS'
{-# LANGUAGE TypeFamilies #-}
module IndependentIndicesClient where
import IndependentIndices
type instance Use Maybe Maybe = Bool
observation :: Use Maybe Maybe
observation = True
HS
    cat >TwoFamilyResultsClient.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers, StandaloneKindSignatures, TypeFamilies #-}
module TwoFamilyResultsClient where
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Exts (U)
import TwoFamilyResults
type instance Use Maybe Maybe Maybe = Bool
observation :: Use Maybe Maybe Maybe
observation = True

type Callback :: Type %U -> Type
type family Callback a where
  Callback Int = Bool
-- A shared parameter index would reject each mixed application.
independent :: Proxy (Arrow Maybe Callback, Arrow Callback Maybe)
independent = Proxy
HS
    args+=(IndependentIndicesClient.hs TwoFamilyResultsClient.hs)
    "${new[@]}" "${args[@]}" >upgrade.log 2>&1 || { cat upgrade.log >&2; exit 1; }
    for owner in IndependentIndices TwoFamilyResults; do
        grep -E "Compiling $owner .*\[Compiler now separates family result and parameter matchability\]" upgrade.log
        grep -E "Compiling ${owner}Client([[:space:]]|$)" upgrade.log
    done
    "${new[@]}" --show-iface ExplicitKind.hi >explicit.new
    grep -F '  ABI hash:' explicit.new >explicit.new.abi
    cmp explicit.old.abi explicit.new.abi
    sha256sum -c unrelated.sha --quiet
    stat -c '%y %n' Unrelated.{hi,o} >unrelated.current
    cmp unrelated.time unrelated.current
    if grep -F 'Compiling Unrelated' upgrade.log; then exit 1; fi

    sha256sum ./*.hi ./*.o >warm.sha
    "${new[@]}" "${args[@]}" >warm.log 2>&1 || { cat warm.log >&2; exit 1; }
    sha256sum -c warm.sha --quiet
    if grep -F 'Compiling ' warm.log; then exit 1; fi
    echo 'The owner interfaces were upgraded, the clients passed, and the second build reused all outputs.'
    exit 0
fi

if [[ -n $boot_dir ]]; then
    old_internal=$(find "$old_lib" -type d -name 'ghc-internal-*-inplace' -print -quit)
    new_internal=$(find "$lib" -type d -name 'ghc-internal-*-inplace' -print -quit)
    archive=$(find "$new_internal" -maxdepth 1 -name 'libHSghc-internal-*-inplace.a' -print -quit)
    replace "$old_internal/GHC/Internal/Err.hi" "$new_internal/GHC/Internal/Err.hi"
    replace "$old_internal/${archive##*/}" "$archive"
    for boot in GHC/Internal/Exception{,/Type}.hi-boot; do
        test -f "$boot_dir/$boot"
    done
    while IFS= read -r boot; do
        dest="$new_internal/${boot#"$boot_dir/"}"
        mkdir -p "$(dirname "$dest")"
        replace "$boot" "$dest"
    done < <(find "$boot_dir/GHC/Internal" -type f -name '*.hi-boot')
    mkdir -p "$work/err/GHC/Internal"
    cp "$repo/libraries/ghc-internal/src/GHC/Internal/Err.hs" "$work/err/GHC/Internal/"
    (
        cd "$work/err"
        err=(-O2 -fignore-interface-pragmas -this-unit-id ghc-internal
             -i -i. "-i$new_internal" "${sanity[@]}" -c GHC/Internal/Err.hs)
        "${old[@]}" "${err[@]}"
        if nm -g --defined-only GHC/Internal/Err.o | grep -E "$new_companions"; then exit 1; fi
        sha256sum GHC/Internal/Err.hs >source.sha
        "${new[@]}" -v1 -ddump-hi-diffs "${err[@]}" >upgrade.log 2>&1 || { cat upgrade.log >&2; exit 1; }
        grep -F 'Compiler now emits native pair readback recipes' upgrade.log
        nm -g --defined-only GHC/Internal/Err.o | grep -E "$new_companions"
        sha256sum -c source.sha --quiet
        sha256sum GHC/Internal/Err.{hi,o} >warm.sha
        "${new[@]}" -v1 "${err[@]}" >warm.log 2>&1
        grep -F 'compilation IS NOT required' warm.log
        sha256sum -c warm.sha --quiet
    )
fi

# Exercise the SOURCE cycle in Note [Synonym kind loop] on the new reader.
(
    mkdir "$work/synonym-loop"
    cd "$work/synonym-loop"
    cp "$repo"/testsuite/tests/typecheck/should_compile/T2412{.hs,.hs-boot,A.hs} .
    "${new[@]}" "${sanity[@]}" -c T2412.hs-boot T2412A.hs T2412.hs
)

cd "$work/home"
cp "$src/PairProjection.hs" .
cat >Scalar.hs <<'HS'
module Scalar (field) where
import Numeric.Natural (Natural)
{-# OPAQUE field #-}
field :: Natural -> Char
field _ = error "upgrade sibling"
HS
cat >Worker.hs <<'HS'
module Worker (bump) where
import Numeric.Natural (Natural)
import qualified Scalar
bump :: Natural -> (Natural, Char)
bump n = (n + 1, Scalar.field n)
HS
cat >Certified.hs <<'HS'
module Certified (field) where
field :: Bool -> Bool
field False = True
field True = False
HS
cat >CertifiedWorker.hs <<'HS'
module CertifiedWorker (bump) where
import Numeric.Natural (Natural)
import qualified Certified
bump :: Natural -> (Natural, Bool)
bump n = (n + 1, Certified.field True)
HS
cat >ClosedProvider.hs <<'HS'
{-# LANGUAGE DataKinds, TypeFamilies #-}
module ClosedProvider (K) where
import Data.Kind (Type)
type family K (b :: Bool) :: Type where
  K 'True = Type -> Type
  K 'False = Bool -> Bool
HS
cat >OpenProvider.hs <<'HS'
{-# LANGUAGE DataKinds, TypeFamilies #-}
module OpenProvider where
import Data.Kind (Type)
class Owner (b :: Bool) where
  type Associated b (a :: Type) :: Type
class Owner b => Observer b where
  type Observe b a (f :: Associated b a) :: Type
HS
{
    printf '%s\n' '{-# LANGUAGE DataKinds, TypeFamilies #-}' 'module Both where' 'import Data.Kind (Type)'
    sed -n '/^type family K /,$p' ClosedProvider.hs
    sed -n '/^class Owner /,$p' OpenProvider.hs
    printf '%s\n' 'field :: Bool -> Bool' 'field b = not b'
} >Both.hs
cat >Unrelated.hs <<'HS'
module Unrelated (answer) where
answer :: Int
answer = 42
HS
cat >Proof.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Proof where
import Data.Type.Equality ((:~:) (Refl))
import qualified PairProjection as P
import qualified Worker as W
proof :: P.pick (W.bump 7) :~: 8
proof = Refl
HS
sed -e 's/module Proof/module CertifiedProof/' -e 's/import qualified Worker/import qualified CertifiedWorker/' Proof.hs >CertifiedProof.hs
opts=(-O0 -fomit-interface-pragmas -fno-write-if-simplified-core -i -i. "${sanity[@]}")
roots=(--make -no-link Worker.hs CertifiedWorker.hs ClosedProvider.hs OpenProvider.hs Both.hs Unrelated.hs PairProjection.hs)
"${old[@]}" "${opts[@]}" "${roots[@]}"
if nm -g --defined-only Scalar.o Worker.o Certified.o Both.o | grep -E "$new_companions"; then exit 1; fi
"${old[@]}" --show-iface Certified.hi >old-certified.iface
grep -F 'ExecutableSourceTable' old-certified.iface
for provider in ClosedProvider OpenProvider Both; do
    "${old[@]}" --show-iface "$provider.hi" >"$provider.old"
    if grep -F 'matchability-schemes-v' "$provider.old"; then exit 1; fi
done

# Binary-only producers cannot be upgraded. The ordinary fallback reports
# the exception when the projection also has no executable object.
cp ./*.hi ./*.o Proof.hs "$work/binary/"
(
    cd "$work/binary"
    rm PairProjection.o
    if "${new[@]}" "${opts[@]}" -c Proof.hs >binary.log 2>&1 ||
       ! grep -Fq 'upgrade sibling' binary.log || ! grep -Fq '[GHC-70565]' binary.log; then
        cat binary.log >&2
        exit 1
    fi
)
# Keep an old interface whose synonym RHS is just the family head.
(
    cd "$work/bare"
    cp ../home/ClosedProvider.hi .
    cat >Saved.hs <<'HS'
{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies #-}
module Saved (Saved) where
import ClosedProvider (K)
type Saved = K
HS
    cat >UseSaved.hs <<'HS'
{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module UseSaved where
import Saved (Saved)
identity :: Saved 'True
identity x = x
HS
    "${old[@]}" "${opts[@]}" -c Saved.hs UseSaved.hs
    rm Saved.hs
    "${new[@]}" "${opts[@]}" -fforce-recomp -c UseSaved.hs
)

if [[ -n $boot_dir ]]; then
    replace "$work/err/GHC/Internal/Err.hi" "$new_internal/GHC/Internal/Err.hi"
    cp "$archive" "$archive.new"
    ar rcs "$archive.new" "$work/err/GHC/Internal/Err.o"
    mv -f "$archive.new" "$archive"
fi

sha256sum ./*.hs Unrelated.{hi,o} PairProjection.{hi,o} >fixed.sha
stat -c '%y %n' Unrelated.{hi,o} PairProjection.{hi,o} >fixed.time
"${new[@]}" "${opts[@]}" -v1 "${roots[@]}" >upgrade.log 2>&1 || { cat upgrade.log >&2; exit 1; }
for provider in Scalar Worker Certified CertifiedWorker; do
    grep -F "Compiling $provider [Compiler now emits native pair readback recipes]" upgrade.log
done
grep -F 'Compiling ClosedProvider [Compiler now infers closed-family matchability]' upgrade.log
grep -F 'Compiling OpenProvider [Compiler now infers open-family matchability]' upgrade.log
grep -F 'Compiling Both [Compiler now infers open-family matchability]' upgrade.log
for provider in ClosedProvider OpenProvider Both; do
    "${new[@]}" --show-iface "$provider.hi" >"$provider.new"
    grep -F 'matchability-schemes-v' "$provider.new"
done
(
    cd "$work/bare"
    cp ../home/ClosedProvider.hi .
    if "${new[@]}" "${opts[@]}" -fforce-recomp -c UseSaved.hs >bare.log 2>&1 ||
       ! grep -Fq 'Incompatible interface dependency:' bare.log ||
       ! grep -Fq 'Rebuild the module that refers to it: Saved' bare.log; then
        cat bare.log >&2
        exit 1
    fi
)
(
    mkdir "$work/remote"
    cd "$work/remote"
    cp ../home/OpenProvider.hi .
    cat >RemoteInstance.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies #-}
module RemoteInstance where
import Data.Kind (Type)
import OpenProvider

instance Owner 'True where
  type Associated 'True a = a -> a

instance Observer 'True where
  type Observe 'True Type Maybe = Bool

identity :: Associated 'True Type
identity x = x
HS
    "${new[@]}" "${opts[@]}" -c RemoteInstance.hs
    rm RemoteInstance.hs
    cat >RemoteClient.hs <<'HS'
{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies #-}
module RemoteClient where
import Data.Kind (Type)
import OpenProvider
import RemoteInstance ()

observation :: Observe 'True Type Maybe
observation = True
HS
    "${new[@]}" "${opts[@]}" -c RemoteClient.hs
)
nm -g --defined-only Scalar.o Worker.o Certified.o | grep -E "$new_companions"
grep -E "type K :: forall.*Matchability" Both.new
grep -E "type Associated :: forall.*Matchability" Both.new
nm -g --defined-only Both.o | grep -E "$new_companions"
sha256sum -c fixed.sha --quiet
stat -c '%y %n' Unrelated.{hi,o} PairProjection.{hi,o} >current.time
cmp fixed.time current.time

sha256sum ./*.hi ./*.o >warm.sha
"${new[@]}" "${opts[@]}" -v1 "${roots[@]}" >warm.log 2>&1
sha256sum -c warm.sha --quiet
if grep -F 'Compiling ' warm.log; then exit 1; fi

mv PairProjection.o PairProjection.saved
"${new[@]}" "${opts[@]}" -c Proof.hs CertifiedProof.hs
"${new[@]}" --show-iface Certified.hi >new-certified.iface
grep -F 'ExecutableSourceTable' new-certified.iface
"${new[@]}" --show-iface CertifiedProof.hi >before-swap.iface
grep -F 'addDependentFile "./Certified.o"' before-swap.iface >before-swap.usage
mkdir replacement
sed -e '/^field False/,$d' Certified.hs >replacement/Certified.hs
printf '%s\n' 'field _ = error "certified sibling"' >>replacement/Certified.hs
(cd replacement && "${new[@]}" "${opts[@]}" -c Certified.hs)
sha256sum Certified.hi CertifiedWorker.{hi,o} >certified.sha
cp replacement/Certified.o .
"${new[@]}" "${opts[@]}" -v1 -c CertifiedProof.hs >certified.log 2>&1 ||
    { cat certified.log >&2; exit 1; }
"${new[@]}" --show-iface CertifiedProof.hi >after-swap.iface
grep -F 'addDependentFile "./Certified.o"' after-swap.iface >after-swap.usage
if cmp -s before-swap.usage after-swap.usage; then exit 1; fi
sha256sum -c certified.sha --quiet
printf '%s\n' 'The certified scalar replacement invalidated the proof and preserved its result.'
