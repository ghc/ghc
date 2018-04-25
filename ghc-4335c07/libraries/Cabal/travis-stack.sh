#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/sh

if [ -z ${STACKAGE_RESOLVER+x} ]; then
    echo "STACKAGE_RESOLVER environment variable not set."
    echo "This build case is not configured correctly."
    exit 1
fi

. ./travis-common.sh

# ---------------------------------------------------------------------
# Build Cabal via Stack(age).
# ---------------------------------------------------------------------

stack build \
    --no-terminal \
    --resolver "$STACKAGE_RESOLVER"
