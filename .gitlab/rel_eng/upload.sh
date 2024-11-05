#!/usr/bin/env nix-shell
#! nix-shell -i bash -p moreutils lzip zip lftp gnupg

set -Eeuo pipefail

# This is a script for preparing and uploading a release of GHC.
#
# Usage,
#   1. Update $ver
#   2. Set $SIGNING_KEY to your key id (prefixed with '=')
#   3. Create a directory and place the source and binary tarballs there
#   4. Run this script from that directory
#
# You can also invoke the script with an argument to perform only
# a subset of the usual release,
#
#   upload.sh recompress             produce lzip tarballs from xz tarballs
#
#   upload.sh gen_hashes             generate signed hashes of the release
#                                    tarballs
#
#   upload.sh prepare_docs           (deprecated) prepare the documentation directory
#                                    (this should be unnecessary as the script which
#                                     fetches artifacts should create this folder from
#                                     the doc-tarball job)
#
#   upload.sh upload_docs            upload documentation to hackage from the hackage_docs folder
#
#   upload.sh upload                 upload the tarballs and documentation
#                                    to downloads.haskell.org
#
# Prerequisites: moreutils

: ${SIGNING_KEY:="=Benjamin Gamari <ben@well-typed.com>"}


# Infer release name from directory name
: ${rel_name:=$(basename $(pwd))}

# Infer version from tarball names
: ${ver:=$(ls ghc-*.tar.* | sed -ne 's/ghc-\([0-9]\+\.[0-9]\+\.[0-9]\+\(\.[0-9]\+\)\?\).\+/\1/p' | head -n1)}
if [ -z "$ver" ]; then echo "Failed to infer \$ver"; exit 1; fi

host="gitlab-storage.haskell.org"

usage() {
    echo "Usage: [rel_name=<name>] ver=7.10.3-rc2 $0 <action>"
    echo
    echo "where,"
    echo "  ver                gives the version number (e.g. the name of the tarballs, in the case of"
    echo "                     a release candidate something like 7.10.3.20150820, otherwise just 7.10.3)"
    echo "  rel_name           gives the release name (e.g. in the case of a release candidate 7.10.3-rc2"
    echo "                     otherwise just 7.10.3)"
    echo "and <action> is one of,"
    echo "  [nothing]          do everything below"
    echo "  recompress         produce lzip and gzip tarballs from xz tarballs"
    echo "  gen_hashes         generated hashes of the release tarballs"
    echo "  sign               sign hashes of the release tarballs"
    echo "  prepare_docs       prepare the documentation directory"
    echo "  upload_docs        upload documentation downloads.haskell.org"
    echo "  upload             upload the tarballs and documentation to downloads.haskell.org"
    echo "  purge_all          purge entire release from the CDN"
    echo "  purge_file file    purge a given file from the CDN"
    echo "  verify             verify the signatures in this directory"
    echo
}

if [ -z "$ver" ]; then
    usage
    exit 1
fi
if [ -z "$rel_name" ]; then
    rel_name="$ver"
fi

# returns the set of files that must have hashes generated.
function hash_files() {
    echo $(find -maxdepth 1 \
         -iname '*.xz' \
      -o -iname '*.gz' \
      -o -iname '*.lz' \
      -o -iname '*.gz' \
      -o -iname '*.bz2' \
      -o -iname '*.zip' \
    )
    echo $(find -maxdepth 1 -iname '*.patch')
}

function gen_hashes() {
    echo -n "Hashing..."
    sha1sum $(hash_files) >| SHA1SUMS &
    sha256sum $(hash_files) >| SHA256SUMS &
    wait
    echo "done"
}

function sign() {
    # Kill DISPLAY lest pinentry won't work
    DISPLAY=
    eval "$(gpg-agent --daemon --sh --pinentry-program $(which pinentry))"
    for i in $(hash_files) SHA1SUMS SHA256SUMS; do
        if [ -e $i -a -e $i.sig -a $i.sig -nt $i ]; then
            echo "Skipping signing of $i"
            continue
        elif [ -e $i.sig ] && gpg2 --verify $i.sig; then
            # Don't resign if current signature is valid
            touch $i.sig
            continue
        fi
        echo "Signing $i"
        rm -f $i.sig
        gpg2 --use-agent --detach-sign --local-user="$SIGNING_KEY" $i
    done
}

function verify() {
    if [ $(find -iname '*.sig' | wc -l) -eq 0 ]; then
        echo "No signatures to verify"
        return
    fi

    for i in *.sig; do
        echo
        echo Verifying $i
        gpg2 --verify $i $(basename $i .sig)
    done
}

function upload() {
    verify
    chmod ugo+r,o-w -R .
    dir=$(echo $rel_name | sed s/-release//)
    lftp -c " \
	    open -u ghc: sftp://$host && \
	    mirror -P20 -c --reverse --exclude=fetch-gitlab --exclude=out . ghc/$dir && \
	    wait all;"
    chmod ugo-w $(ls *.xz *.bz2 *.zip)
}

function purge_all() {
    local dir="$(echo $rel_name | sed s/-release//)"
    # Purge CDN cache
    curl -X PURGE http://downloads.haskell.org/ghc/
    curl -X PURGE http://downloads.haskell.org/~ghc/
    curl -X PURGE http://downloads.haskell.org/ghc/$dir
    curl -X PURGE http://downloads.haskell.org/ghc/$dir/
    curl -X PURGE http://downloads.haskell.org/~ghc/$dir
    curl -X PURGE http://downloads.haskell.org/~ghc/$dir/
    for i in *; do
        purge_file "$i"
    done
}

function purge_file() {
    dirs=(
        "~ghc/$rel_name"
        "ghc/$rel_name"
        "~ghc/$ver"
        "ghc/$ver"
    )

    for dir in ${dirs[@]}; do
        curl -X PURGE http://downloads.haskell.org/$dir/$1
        curl -X PURGE http://downloads.haskell.org/$dir/$1/
        curl -X PURGE http://downloads.haskell.org/$dir/$1/docs/
    done
}

function prepare_docs() {
    echo "THIS COMMAND IS DEPRECATED, THE DOCS FOLDER SHOULD BE PREPARED BY THE FETCH SCRIPT"
}

function recompress() {
    set -Eeuo pipefail
    needed=()

    for i in ghc-*.tar.xz; do
        needed+=( "$(basename $i .xz).gz" )
    done

    for i in ghc-*-darwin.tar.xz; do
        needed+=( "$(basename $i .xz).bz2" )
    done

    for i in ghc-*-mingw32.tar.xz; do
        needed+=( "$(basename $i .tar.xz).zip" )
    done

    recompress-all -j10 ${needed[@]}
}

function upload_docs() {
    local tmp="$(mktemp -d)"
    tar -xf ghc-$ver-src.tar.xz -C "$tmp"
    GHC_TREE="$tmp/ghc-$ver"
    local args=$@
    if [[ -n "$PUBLISH" ]]; then
        echo "Publishing to Hackage..."
        args+=( "--publish" )
    fi
    "$GHC_TREE/.gitlab/rel_eng/upload_ghc_libs.py" upload --docs=hackage_docs ${args[@]}
}

if [ "x$1" == "x" ]; then
    recompress
    gen_hashes
    sign
    if [ ! -d docs ]; then
        prepare_docs || ( rm -R docs; exit 1 )
    fi
    if [ -d hackage_docs ]; then
      upload_docs
    fi
    upload
    purge_all
else
    $@
fi
