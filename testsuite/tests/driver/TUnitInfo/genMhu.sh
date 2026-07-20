#! /usr/bin/env bash

set -euo pipefail

HOME_UNITS=${1:-50}

unit_dir()  { echo "p$1"; }
unit_fname(){ echo "unitp$1"; }
mod_name() { echo "Mod$1"; }

mk_unit_file() {
    local p=$1
    echo "-clear-package-db -global-package-db -no-user-package-db -working-dir $(unit_dir "$p") -this-unit-id $(unit_dir "$p") $(mod_name "$p") ${deps[*]}" \
        > "$(unit_fname "$p")"
}

mk_top_mod() {
    local p=$1
    echo "module $(mod_name "$p") where" > "$(unit_dir "$p")/$(mod_name "$p").hs"
}

for ((p = 0; p < HOME_UNITS; p++)); do
    mkdir "$(unit_dir "$p")"
    mk_unit_file "$p"
    mk_top_mod "$p"
done
