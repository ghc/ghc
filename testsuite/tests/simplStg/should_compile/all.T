# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# Only compile with optimisation
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('T13588', [ grep_errmsg('case') ] , compile, ['-dverbose-stg2stg -fno-worker-wrapper'])
