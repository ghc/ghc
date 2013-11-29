# This directory contains tests where we annotate functions with expected
# type signatures, and verify that these actually those found by the compiler

def f(name, opts):
  if (ghc_with_interpreter == 0):
	opts.skip = 1

setTestOpts(f)
setTestOpts(when(compiler_lt('ghc', '7.1'), skip))
setTestOpts(extra_clean(['StrAnalAnnotation.hi','StrAnalAnnotation.o']))

# We are testing the result of an optimization, so no use
# running them in various runtimes
setTestOpts(only_ways(['optasm']))

# Use this as a template
test('StrAnalExample', normal, compile, ['--make -package ghc -v0 ' + config.ghc_th_way_flags])

test('T8569', expect_broken(8569), compile, ['--make -package ghc -v0 ' + config.ghc_th_way_flags])

