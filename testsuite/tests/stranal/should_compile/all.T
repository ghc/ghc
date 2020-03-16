# Only compile with optimisation
setTestOpts( only_ways(['optasm']) )

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

test('T9208', when(compiler_debugged(), expect_broken(9208)), compile, [''])
# T9208 fails (and should do so) if you have assertion checking on in the compiler
# Hence the above expect_broken.  See comments in the ticket

test('T10694', [ grep_errmsg(r'(Str|Cpr)=') ],   compile, ['-dppr-cols=200 -ddump-simpl'])
test('T11770', [ check_errmsg('OneShot') ], compile, ['-ddump-simpl'])

test('T13031', normal, makefile_test, [])

test('T13077', normal, compile, [''])
test('T13077a', normal, compile, [''])

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
