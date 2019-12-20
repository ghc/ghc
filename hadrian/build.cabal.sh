#!/usr/bin/env bash

CABAL=${CABAL-cabal}
CABFLAGS="$CABFLAGS"

# It is currently more robust to pass Cabal an absolute path to the project file.
PROJ="$PWD/hadrian/cabal.project"

set -euo pipefail

if ! [ -f "$PROJ" ]; then
    echo "Current working directory must be GHC's top-level folder"
    exit 2
fi

if ! type "$CABAL" > /dev/null; then
    echo "Please make sure 'cabal' is in your PATH"
    exit 2
fi

CABVERSTR=$("$CABAL" --numeric-version)
IFS="." read -ra CABVER <<< "$CABVERSTR"

build_failed() {
    ( ghc --info | grep -s '("Support SMP","YES")' > /dev/null ) \
      || cat <<EOF
Your compiler does not support the threaded runtime system.
Please disable the \`threaded\` Cabal flag in project.cabal.local
by running:

    echo -e "package hadrian\n  flags: -threaded" >> project.cabal.local

EOF
    exit 1
}

if [ "${CABVER[0]}" -lt 2 ] || [ "${CABVER[0]}" -eq 2 ] && [ "${CABVER[1]}" -lt 2 ];
then
    echo "Cabal version is too old; you need at least cabal-install 2.2"
    exit 2
fi

# shellcheck disable=SC2086
"$CABAL" --project-file="$PROJ" v2-build $CABFLAGS -j exe:hadrian

# use new-exec instead of new-run to make sure that the build-tools (alex & happy) are in PATH
# shellcheck disable=SC2086
"$CABAL" --project-file="$PROJ" v2-exec  $CABFLAGS    hadrian -- \
    --directory "$PWD" \
    "$@" \
    || build_failed
