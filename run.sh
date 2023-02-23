test=testsuite/tests/driver/T20030/test1

build="_build"
tree="$HOME/ghc/$build"

cmdline=(
	"$tree/stage1/bin/ghc"
       	--make -i$test I K
	-dcore-lint -dstg-lint -dcmm-lint -no-user-package-db -fno-dump-with-ways
	-rtsopts
	-v1 -j8 -fforce-recomp +RTS -DZ $RTS_FLAGS -RTS
)

run() {
    $WRAPPER "${cmdline[@]}" $@ || return 1
}

run_many() {
    local i=0
    while true; do
	let i=i+1
	printf "# Iteration %d\n" "$i"
	run $@ || break
    done
}

run_rr() {
    rm -Rf rr.out
    WRAPPER="$HOME/rr/bin/rr record -h -o rr.out" \
	    run $@
}

run_many_rr() {
    local i=0
    while true; do
	let i=i+1
	printf "# Iteration %d\n" "$i"
        run_rr $@ || break
    done
}

run_gdb() {
    gdb --args "${cmdline[@]}" $@
}

build() {
    local args=(
        "-j4"
       	"stage1.*.ghc.c.opts+=-optc-moutline-atomics"
	"stage1.rts.ghc.hs.opts+=-ddump-to-file -ddump-cmm -ddump-asm"
    )
    hadrian/build-cabal -o_build --flavour=default+debug_info+debug_ghc "${args[@]}" &
    hadrian/build-cabal -o_build-llvm --flavour=default+debug_info+debug_ghc+llvm "${args[@]}" &
    wait
}

mode=$1
shift
$mode $@
