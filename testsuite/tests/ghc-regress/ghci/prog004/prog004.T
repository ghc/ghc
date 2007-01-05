setTestOpts(only_compiler_types(['ghc']))

def f(opts):
  if not ('ghci' in config.run_ways):
	opts.skip = 1
setTestOpts(f)

test('ghciprog004', ignore_output, run_command, ['$MAKE ghciprog004'])
clean(['ctest.c', 'ctest.o'])
