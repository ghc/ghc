setTestOpts(only_compiler_types(['ghc']))

def f(name, opts):
  if not ('ghci' in config.run_ways):
	opts.skip = 1
setTestOpts(f)

test('ghciprog004',
     extra_clean(['ctest.c', 'ctest.o']),
     run_command,
     ['$MAKE -s --no-print-directory ghciprog004'])
