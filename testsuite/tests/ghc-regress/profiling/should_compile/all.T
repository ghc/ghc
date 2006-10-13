
def f( opts ):
  opts.only_ways = filter(lambda x: 'prof' in x, config.run_ways)
setTestOpts(f)

# We need to run prof001 and prof002 all ways, as the compile succeeds
# due to the extra flags added for the profiling ways.
test('prof001', only_ways([]), compile, ['-prof -caf-all'])
test('prof002', only_ways([]), compile, ['-prof -caf-all'])

