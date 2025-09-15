# Just do the opt way...
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('CPRRepeat', normal, compile_and_run, [''])
test('T20598', normal, multimod_compile_and_run, ['T20598c', ''])
