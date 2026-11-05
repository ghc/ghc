#!/usr/bin/env bash
# Re-vendor the filecheck package from its pinned upstream release.
# See README.md in this directory for the rules; to update, bump TAG and
# COMMIT, run this script, review the diff, commit.
set -euo pipefail

UPSTREAM=https://github.com/AntonLydike/filecheck
TAG=v1.0.6
COMMIT=0b78bf8e9eebb34712f3e934907700251fb82878

cd "$(dirname "$0")"
root=$(git rev-parse --show-toplevel)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

git -c advice.detachedHead=false clone --quiet --depth 1 --branch "$TAG" "$UPSTREAM" "$tmp/src"
head=$(git -C "$tmp/src" rev-parse HEAD)
if [ "$head" != "$COMMIT" ]; then
    echo "$0: $TAG resolves to $head, expected $COMMIT" >&2
    exit 1
fi

rm -rf filecheck
cp -R "$tmp/src/filecheck" filecheck
cp "$tmp/src/LICENSE.txt" filecheck/
cat > filecheck/UPSTREAM <<STAMP
Vendored verbatim by $(basename "$0"); do not edit, see ../README.md.
$UPSTREAM
tag $TAG, commit $COMMIT
STAMP

# Local changes, kept as patches until they are released upstream.
for p in patches/filecheck-*.patch; do
    [ -e "$p" ] || continue
    git -C "$root" apply "$PWD/$p"
done
