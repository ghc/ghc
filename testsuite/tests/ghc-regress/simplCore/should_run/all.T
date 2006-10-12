# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# Only compile with optimisation
def f( opts ):
  opts.only_ways = ['opt']

setTestOpts(f)

test('simplrun001', normal, compile_and_run, [''])
test('simplrun002', normal, compile_and_run, [''])
test('simplrun003', normal, compile_and_run, [''])
test('simplrun004', normal, compile_and_run, [''])
test('simplrun005', normal, compile_and_run, [''])
test('simplrun006', normal, compile_and_run, [''])
