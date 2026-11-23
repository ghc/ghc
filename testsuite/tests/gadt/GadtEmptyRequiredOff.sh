#!/usr/bin/env bash
set -euo pipefail

ghc=("$1" "-B$2" -v0 -package-env - -fno-code -fforce-recomp -ferror-spans -fdiagnostics-color=never)
implied=-XExperimentalDependentTypes
explicit=-XRequiredTypeArguments
off=-XNoRequiredTypeArguments
vdq='[GHC-51580]'

check_location() {
    grep -F "GadtEmptyRequiredOff.hs:$2" "$1" | grep -Fq "$vdq" || { cat "$1"; return 1; }
}

check() {
    local shape=$1 name=$2 expected=$3 status=0
    shift 3
    local log="$shape-$name.log"
    "${ghc[@]}" "$@" "-DCHECK_$shape" GadtEmptyRequiredOff.hs > "$log" 2>&1 || status=$?
    if [[ -z "$expected" ]]; then
        test "$status" -eq 0 || { cat "$log"; return 1; }
    else
        test "$status" -eq 1 && grep -Fq "$expected" "$log" || { cat "$log"; return 1; }
        case "$shape" in
            SOURCE)
                check_location "$log" 10:14
                check_location "$log" 13:20
                ;;
            QUOTE)
                check_location "$log" 19:33
                check_location "$log" 20:39
                ;;
        esac
    fi
}

for shape in SOURCE QUOTE SPLICE_LEADING SPLICE_LATE SPLICE_STRUCTURED SPLICE_GADTC TYPE_DATA TERM_REQUIRED; do
    check "$shape" default "$vdq"
    check "$shape" explicit '' "$explicit"
    check "$shape" off "$vdq" "$off"
    check "$shape" implied '' "$implied"
    check "$shape" override "$vdq" "$implied" "$off"
    check "$shape" reverse '' "$off" "$implied"
done

# Preserve the ordinary term grammar and the extension's lexical capture rules.
check TERM_EMPTY default ''
check TERM_EMPTY override '' "$implied" "$off"
check SPLICE_FIELD default ''
check SPLICE_FIELD override '' "$implied" "$off"
check CAPTURE default ''
check CAPTURE explicit 'term variables cannot be promoted' "$explicit"
check CAPTURE implied 'term variables cannot be promoted' "$implied"
check CAPTURE override '' "$implied" "$off"
