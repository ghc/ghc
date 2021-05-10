# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# Only compile with optimisation
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('T9291', normal, compile_and_run, [''])
test('T13536', normal, compile_and_run, [''])

test('T13536a',
        [ collect_stats('bytes allocated',5),
     only_ways(['optasm'])],
    compile_and_run,
    [''])

test('inferTags001', normal, multimod_compile_and_run, ['inferTags001', 'inferTags001_a'])
