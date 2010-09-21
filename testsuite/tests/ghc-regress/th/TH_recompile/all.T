def f(opts):
  opts.extra_hc_opts = '-XTemplateHaskell -package template-haskell'
  if (ghc_with_interpreter == 0):
	opts.skip = 1

setTestOpts(f)
setTestOpts(only_compiler_types(['ghc']))
setTestOpts(only_ways(['normal']));
setTestOpts(if_compiler_profiled(skip))

test('TH_recompile',
     [expect_broken(481),
      clean_cmd('$MAKE -s clean')],
     run_command,
     ['$MAKE -s TH_recompile'])

