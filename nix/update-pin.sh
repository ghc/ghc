#!/bin/sh
usage() {
        echo 'Usage: USER [REPO] [REV]  Pin a Github REPO to USER/REV,\n  Defaults: REPO=nixpkgs, REV=HEAD-if-CWD-is-nixpkgs' >&2
        exit 1
}
set -eu

jq="$(nix-build --no-build-output --no-out-link '<nixpkgs>' -A jq)/bin/jq"
nixroot="$(realpath $0 | xargs dirname)"
cwdRepo="$(basename $(pwd))"

repo=${1:-'nixpkgs'}
git_rev=${2:-$(if test "${cwdRepo}" = "${repo}"; then git rev-parse HEAD; fi)}
test -n "${git_rev}" || usage

repo_pin_file="${nixroot}"/pins/${repo}.src-json
repo_url=$(${jq} '.url' < "${repo_pin_file}" | xargs echo)
url=${repo_url}"/archive/"${git_rev}.tar.gz

tmp="$(mktemp update-pin.XXXXXXXX)"
cleanup() {
        rm -f "${tmp}"
}
trap cleanup EXIT

cat >&2 <<EOF
-- Updating Nix pin for ${repo} to:  ${git_rev}
-- Snapshot URL:  ${url}
EOF
new_sha256=$(nix-prefetch-url --unpack --type sha256 ${url})
test -n "${new_sha256}" || {
        echo "ERROR:  failed to hash ${url}, is ${git_rev} valid for ${repo_url}?" >&2
        exit 1
}
${jq} ".rev=\"${git_rev}\" | .sha256=\"${new_sha256}\"" < "${repo_pin_file}" | tee "${tmp}"

if test -n "$(cat ${tmp})"
then mv "${tmp}" "${nixroot}"/pins/${repo}.src-json
else echo "ERROR:  pin update failed" >&2; rm -f "${tmp}"; exit 1
fi
