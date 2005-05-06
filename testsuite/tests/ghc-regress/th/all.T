def f(opts):
  opts.extra_hc_opts = '-fth -package template-haskell'
  if not ('ghci' in config.run_ways):
	opts.skip = 1

setTestOpts(f)
setTestOpts(only_compiler_types(['ghc']))
setTestOpts(only_ways(['normal']));

test('TH_repE1', normal, compile, [''])
test('TH_repE2', normal, compile_and_run, [''])
test('TH_repE3', normal, compile, [''])
test('TH_repPrim', normal, compile, [''])
test('TH_repPrimOutput', normal, compile_and_run, [''])
test('TH_repGuard', normal, compile, [''])
test('TH_repGuardOutput', normal, compile_and_run, [''])
test('TH_repPatSig', normal, compile, [''])

test('TH_reifyDecl1', normal, compile, [''])

test('TH_reifyType1', normal, compile, [''])
test('TH_reifyType2', normal, compile, [''])

test('TH_spliceDecl1', normal, compile, ['-v0'])
test('TH_spliceDecl2', normal, compile, ['-v0'])
test('TH_spliceDecl3', normal, multimod_compile, ['TH_spliceDecl3', '-v0'])
clean(['TH_spliceDecl3_Lib.hi', 'TH_spliceDecl3_Lib.o'])

test('TH_spliceE1', normal, compile_and_run, [''])
test('TH_spliceExpr1', normal, compile, ['-v0'])
test('TH_spliceE3', normal, compile, [''])
test('TH_spliceE4', normal, compile_and_run, [''])

test('TH_bracket1', normal, compile, [''])
test('TH_bracket2', normal, compile, [''])
test('TH_bracket3', normal, compile, [''])

test('TH_class1', normal, compile, [''])
test('TH_tuple1', normal, compile, [''])
test('TH_genEx', normal, multimod_compile, ['TH_genEx', '-ddump-splices'])

test('TH_where', normal, compile_and_run, [''])
