# Just do the opt way...
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('CPRRepeat', normal, compile_and_run, [''])
