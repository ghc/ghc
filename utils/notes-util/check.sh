#!/bin/sh

set -e

CABAL_INSTALL="${CABAL_INSTALL:-cabal}"
GHC="${GHC:-ghc}"

cd "$(dirname $0)"
"$CABAL_INSTALL" build -w "$GHC"
bin="$("$CABAL_INSTALL" list-bin -w "$GHC" ghc-notes)"
cd "$(git rev-parse --show-toplevel)"
"$bin" broken-refs \
    | grep -v "utils/notes-util/expected-broken-note-refs:" \
    | sed 's/:[0-9]\+:[0-9]\+:/:/' \
    > broken-note-refs

if diff -q utils/notes-util/expected-broken-note-refs broken-note-refs; then
    printf "No unexpected broken note references"
else
    printf "Found unexpected broken note references:\n\n"
    diff -u utils/notes-util/expected-broken-note-refs broken-note-refs || true
    if [[ "$1" == "-a" ]]; then
        cp broken-note-refs utils/notes-util/expected-broken-note-refs
        printf "\n"
        printf "Accepted new broken note references."
    else
        exit 1
    fi
fi

