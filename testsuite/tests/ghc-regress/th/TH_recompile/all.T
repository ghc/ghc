def f(opts):
  opts.extra_hc_opts = '-fth -package template-haskell'
  if (ghc_with_interpreter == 0):
	opts.skip = 1

setTestOpts(f)
setTestOpts(only_compiler_types(['ghc']))
setTestOpts(only_ways(['normal']));

test('TH_recompile', normal, run_command, ['$MAKE -s TH_recompile'])

if default_testopts.cleanup != '':
    runCmd('$MAKE -C ' + in_testdir('') + ' clean')

