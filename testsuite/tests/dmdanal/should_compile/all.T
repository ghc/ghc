# Only compile with optimisation
setTestOpts( only_ways(['optasm']))
setTestOpts( extra_hc_opts('-dno-debug-output') )

test('default', normal, compile, [''])
test('fact', normal, compile, [''])
test('fun', normal, compile, [''])
test('goo', normal, compile, [''])
test('ins', normal, compile, [''])
test('map', normal, compile, [''])
test('sim', normal, compile, [''])
test('str001', normal, compile, [''])
test('str002', normal, compile, [''])
test('syn', normal, compile, [''])
test('test', normal, compile, [''])
test('tst', normal, compile, [''])
test('unu', normal, compile, [''])
test('newtype', req_profiling, compile, ['-prof -fprof-auto'])
test('T1988', normal, compile, [''])
test('T8467', normal, compile, [''])
test('T8037', normal, compile, [''])
test('T8743', [], multimod_compile, ['T8743', '-v0'])

# T10482
#   The intent here is to check that $wfoo has type
#         $wfoo :: Int# -> Int# -> Int
#   with two unboxed args.  See #10482 for background
#
# Set -dppr-cols to ensure output doesn't wrap
test('T10482',  [ grep_errmsg(r'wfoo.*Int#') ], compile, ['-dppr-cols=200 -ddump-simpl'])
test('T10482a', [ grep_errmsg(r'wf.*Int#') ],   compile, ['-dppr-cols=200 -ddump-simpl'])

test('T9208', normal, compile_fail, ['-dstg-lint -O -ddebug-output'])
# T9208 fails (and should do so) if you have assertion checking on in the compiler
# It now also fails with stgLint, hence the above compile_fail.  See comments in the ticket

test('T10694', [ grep_errmsg(r'(Str|Cpr)=') ],   compile, ['-dppr-cols=200 -ddump-simpl -dsuppress-uniques'])
test('T11770', [ check_errmsg('OneShot') ], compile, ['-ddump-simpl'])

test('T13031', normal, makefile_test, [])

test('T13077', normal, compile, [''])
test('T13077a', normal, compile, [''])

# T13143: WW for NOINLINE function f
test('T13143', [ grep_errmsg(r'^T13143\.\$wf') ], compile, ['-ddump-simpl -dsuppress-uniques'])

# T15627
#   Absent bindings of unlifted types should be WW'ed away.
#   The idea is to check that both $wmutVar and $warray
#   don't mention MutVar# and Array# anymore.
test('T15627',  [ grep_errmsg(r'(wmutVar|warray).*Int#') ], compile, ['-dppr-cols=200 -ddump-simpl'])
# Absent bindings of unlifted newtypes are WW’ed away.
test('T17852',  [ grep_errmsg(r'\\$wf ::') ], compile, ['-ddump-worker-wrapper -dsuppress-idinfo'])

test('T16029', normal, makefile_test, [])
test('T10069',  [ grep_errmsg(r'(wc1).*Int#$') ], compile, ['-dppr-cols=200 -ddump-simpl'])
test('T13380b',  [ grep_errmsg('bigDeadAction') ], compile, ['-dppr-cols=200 -ddump-simpl'])

# We just want to find the worker of foo in there:
test('T18122',  [ grep_errmsg(r'wfoo =') ], compile, ['-ddump-simpl'])
# We just want to find the workers of foo and bar in there:
test('T17819',  [ grep_errmsg(r'(wfoo|wbar)') ], compile, ['-ddump-simpl'])

# We care about the call demand on $wg
test('T18903',  [ grep_errmsg(r'Dmd=\S+C\S+') ], compile, ['-ddump-simpl -dsuppress-uniques'])
# We care about the call demand on $wg1 and $wg2
test('T18894',  [ grep_errmsg(r'Dmd=\S+C\S+') ], compile, ['-ddump-dmdanal -dsuppress-uniques'])
# We care about the Arity 2 on eta, as a result of the annotated Dmd
test('T18894b',  [ grep_errmsg(r'Arity=2') ], compile, ['-ddump-dmdanal -dsuppress-uniques -fno-call-arity -dppr-cols=200'])
# We care about the workers of f,g,h,i:
test('T18982',  [ grep_errmsg(r'\$w. .*Int#$') ], compile, ['-dppr-cols=1000 -ddump-simpl -dsuppress-idinfo -dsuppress-uniques'])
# We care about the type of $wyeah. The unlifted Gram constructor should unbox
test('T20663', [ grep_errmsg(r'\$wyeah ::') ], compile, ['-dppr-cols=1000 -ddump-simpl -dsuppress-idinfo -dsuppress-uniques'])

test('T19180', normal, compile, [''])
test('T19766', [ grep_errmsg(r'absentError') ], compile, ['-ddump-worker-wrapper'])
test('T19849', normal, compile, [''])
test('T19882a', normal, compile, [''])
test('T19882b', normal, compile, [''])
# We want that the 'go' joinrec in the unfolding has been worker/wrappered.
# So we simply grep for 'jump $wgo' and hope we find more than 2 call sites:
test('T20510', [ grep_errmsg(r'jump \$wgo') ], compile, ['-dsuppress-uniques -ddump-exitify'])
test('T20817', [ grep_errmsg(r'Str') ], compile, ['-dsuppress-uniques -ddump-dmdanal'])
# T21150: Check that t{,1,2} haven't been inlined.
test('T21150', [ grep_errmsg(r'( t\d? :: Int)') ], compile, ['-dsuppress-uniques -ddump-exitify'])

# T21128: Check that y is not reboxed in $wtheresCrud
#         If so, there should be no `let` for y
test('T21128', [ grep_errmsg(r'let { y = I\#') ], multimod_compile, ['T21128', '-v0 -dsuppress-uniques -dsuppress-all -ddump-simpl'])

test('T21265', normal, compile, [''])
test('EtaExpansion', normal, compile, [''])
test('T22039', normal, compile, [''])
# T22388: Should see $winteresting but not $wboring
test('T22388', [ grep_errmsg(r'^\S+\$w\S+') ], compile, ['-dsuppress-uniques -ddump-simpl'])
# T22997: Just a panic that should not happen
test('T22997', normal, compile, [''])
test('T23398', normal, compile, ['-dsuppress-uniques -ddump-simpl -dno-typeable-binds'])
test('T24623', normal, compile, [''])
