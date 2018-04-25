#!/nix/store/nkq0n2m4shlbdvdq0qijib5zyzgmn0vq-bash-4.4-p12/bin/bash

# Simple runner script used for TravisCI (where GHC's testsuite runner isn't available)

set -e

GHCVER=$(ghc --numeric-version)

echo "using GHC version: $GHCVER"

# checks if GHC version >= $1
ghc_minver () {
    [ "$1" = "$(echo -e "$1\n${GHCVER}" | sort -V | head -n1)" ]
}

# hard-coded exceptions
may_fail () {
    if [ "$1" = "stm064" ] && ! ghc_minver "7.6.2"; then
        echo "EXPECTED FAIL: '$1' may fail for GHC < 7.6.2"
        return 0
    fi

    return 1
}

die () {
    echo "ERROR: $1" >&2
    exit 1
}

[ -f tests/runtests.sh ] && cd tests/

[ -f runtests.sh ] || die "must be called from inside tests folder"

for T in *.hs;do
    T=${T%.hs}

    echo "== running test '$T'"

    ghc --make -threaded -O2 --make ${T}.hs

    if ./${T} > ${T}.stdout.run 2> ${T}.stderr.run
    then
        echo "${T} exited with code $?"
    fi

    for FD in stdout stderr; do
        if [ -f "${T}.${FD}.ignore" ]; then
            echo "ignoring ${FD} output"
            continue
        fi

        # fixup typo in exception message for older GHCs
        sed -i 's,Transacional invariant,Transactional invariant,g' "${T}.${FD}.run"

        echo "validate ${FD} output..."
        if [ -f "${T}.${FD}" ]; then REF="${T}.${FD}"; else REF=/dev/null; fi

        diff -w -u "${REF}" "${T}.${FD}.run" || may_fail "${T}"
    done

    echo "> '${T}' PASSED"

    rm ${T}.hi ${T}.o ${T} ${T}.stdout.run ${T}.stderr.run
done

echo "----------------------------------------------------------------------------"
echo "all tests PASSED!"
