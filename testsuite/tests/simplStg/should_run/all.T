# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# Only run optimised code, native and interpreted:
# We now generate bytecode from STG so we should test native and bytecode.
def f( name, opts ):
  opts.only_ways = ['optasm', 'ghci-opt']

setTestOpts(f)

# Breakpoint ticks would break CSE which this tests.
test('T9291', [omit_ways(['ghci']), extra_hc_opts('-fno-break-points')], compile_and_run, [''])
test('T13536', normal, compile_and_run, [''])

test('T13536a',
        [ collect_stats('bytes allocated',5),
     only_ways(['optasm'])],
    compile_and_run,
    [''])

test('inferTags001', normal, multimod_compile_and_run, ['inferTags001', 'inferTags001_a'])
test('T22042', [extra_files(['T22042a.hs']),only_ways('normal'),unless(have_dynamic(), skip)], makefile_test, ['T22042'])

# Depends on tag inference being similarly effective for bytecode which it is not yet.
test('T23783', omit_ghci, multimod_compile_and_run, ['T23783', '-O -v0'])

# Requires UNPACK which needs -O
test('unpack_enum', omit_ways(['ghci']), compile_and_run, [''])

# Tests that tag inference optimizes well, which is not the case for bytecode or
# when we insert profiling ticks.
test('T27005a', omit_ways(ghci_ways + prof_ways), compile_and_run, [''])
test('T27831', omit_ways(ghci_ways + prof_ways), compile_and_run, [''])
test('inferTags007', [extra_files(['InferTags007_A.hs'])], multimod_compile_and_run, ['inferTags007', '-O -v0'])
test('T27857', normal, compile_and_run, [''])
