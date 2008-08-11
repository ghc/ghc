# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# Only compile with optimisation
def f( opts ):
  opts.only_ways = ['optc','optasm']

setTestOpts(f)

test('simplrun001', normal, compile_and_run, [''])
test('simplrun002', normal, compile_and_run, [''])
test('simplrun003', normal, compile_and_run, [''])
test('simplrun004', normal, compile_and_run, [''])
test('simplrun005', normal, compile_and_run, [''])
test('simplrun006', expect_broken(149), compile_and_run, [''])
test('simplrun007', normal, compile_and_run, [''])
test('simplrun008', normal, compile_and_run, [''])
test('simplrun009', normal, compile_and_run, [''])
test('simplrun010', composes([extra_run_opts('24 16 8'),
                              exit_code(1)])
                  , compile_and_run, [''])

# Really we'd like to run T2486 too, to check that its
# runtime has not gone up, but here I just compile it so that
# the output of -ddump-rules can be compared
test('T2486', normal, compile, [''])
