test=testsuite/tests/driver/T20030/test1

cmdline=(
	"$HOME/ghc/_build/stage1/bin/ghc"
       	--make -i$test I K
	-dcore-lint -dstg-lint -dcmm-lint -no-user-package-db -fno-dump-with-ways
	-rtsopts
	-v1 -j8 -fforce-recomp +RTS -DS -DZ -N8 -A128k $RTS_FLAGS -RTS
)

run() {
    $WRAPPER "${cmdline[@]}" || return 1
}

run_many() {
    while true; do run || break; done
}

run_rr() {
    rm -Rf $test/rr.out
    WRAPPER="$HOME/rr/bin/rr record -h -o rr.out" \
	    RTS_FLAGS=-qn1 \
	    run
}

run_many_rr() {
    while true; do run_rr || break; done
}

run_gdb() {
    gdb --args "${cmdline[@]}"
}

run_many
