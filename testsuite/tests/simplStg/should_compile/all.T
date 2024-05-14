# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# Only compile with optimisation
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('T13588', [ grep_errmsg('case') ] , compile, ['-dverbose-stg2stg -fno-worker-wrapper'])
test('T15226b', normal, compile, ['-O -ddump-stg-final -dsuppress-uniques -dno-typeable-binds'])
test('T19717', normal, compile, ['-ddump-stg-final -dsuppress-uniques -dno-typeable-binds'])
test('inferTags002', [ only_ways(['optasm']), grep_errmsg(r'(call stg\_ap\_0)', [1])], compile, ['-ddump-cmm -dsuppress-uniques -dno-typeable-binds -O'])

test('T22212', normal, compile, ['-O'])
test('T22840', [extra_files(
        [ 'T22840A.hs'
        , 'T22840B.hs'
        ]), when(not(have_dynamic()),skip)], multimod_compile, ['T22840', '-dynamic-too -dtag-inference-checks'])
test('inferTags003', [ only_ways(['optasm']),
                       grep_errmsg(r'(call stg\_ap\_0)', [1])
                     ], compile, ['-ddump-cmm -dno-typeable-binds -O'])
test('inferTags004', normal, compile, ['-O -ddump-stg-tags -dno-typeable-binds -dsuppress-uniques'])

test('T24806', grep_errmsg('^\\(T24806\\.\\$wgo'), compile, ['-O -ddump-stg-tags -dno-typeable-binds -dsuppress-uniques'])
