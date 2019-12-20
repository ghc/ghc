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
IFS='.' read -ra CABVER <<< "$CABVERSTR"

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

if [ "${CABVER[0]}" -lt 3 ] || [ "${CABVER[0]}" -eq 3 ] && [ "${CABVER[1]}" -lt 2 ];
then
    echo "Cabal version is too old; you need at least cabal-install 3.2"
    exit 2
fi

# Generate local cabal config
CABALHOME="$PWD/hadrian/.cabal"
mkdir -p "$CABALHOME"

cat <<EOF > "$CABALHOME/config"
repository local-hadrian-deps
  url: file+noindex://$PWD/hadrian/repo

remote-build-reporting: anonymous
remote-repo-cache:      $CABALHOME/packages

write-ghc-environment-files: always

build-summary:     $CABALHOME/logs/build.log
extra-prog-path:   $CABALHOME/bin
installdir:        $CABALHOME/bin
logs-dir:          $CABALHOME/logs
store-dir:         $CABALHOME/store
symlink-bindir:    $CABALHOME/bin
world-file:        $CABALHOME/world

install-dirs user
  prefix: $CABALHOME
EOF

export CABAL_CONFIG="$CABALHOME/config"

# shellcheck disable=SC2086
"$CABAL" --project-file="$PROJ" new-build $CABFLAGS -j exe:hadrian

# use new-exec instead of new-run to make sure that the build-tools (alex & happy) are in PATH
# shellcheck disable=SC2086
"$CABAL" --project-file="$PROJ" new-exec  $CABFLAGS    hadrian -- \
    --directory "$PWD" \
    "$@" \
    || build_failed
