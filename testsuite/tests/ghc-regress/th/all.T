def f(opts):
  opts.extra_hc_opts = '-fth -package template-haskell'
  if (ghc_with_interpreter == 0):
	opts.skip = 1

setTestOpts(f)
setTestOpts(only_compiler_types(['ghc']))
setTestOpts(only_ways(['normal']));

test('TH_mkName', normal, compile, [''])

test('TH_repE1', normal, compile, [''])
test('TH_repE2', normal, compile_and_run, [''])
test('TH_repE3', normal, compile, [''])
test('TH_repPrim', normal, compile, [''])
test('TH_repPrimOutput', normal, compile_and_run, [''])
test('TH_repGuard', normal, compile, [''])
test('TH_repGuardOutput', normal, compile_and_run, [''])
test('TH_repPatSig', normal, compile_fail, [''])

test('TH_spliceE5', normal, multimod_compile_and_run, ['TH_spliceE5', '-v0'])
clean(['TH_spliceE5_Lib.hi', 'TH_spliceE5_Lib.o'])

test('TH_spliceD1', normal, multimod_compile_fail, ['TH_spliceD1', '-v0'])
clean(['TH_spliceD1_Lib.hi', 'TH_spliceD1_Lib.o'])

test('TH_spliceD2', normal, multimod_compile, ['TH_spliceD2', '-v0'])
clean(['TH_spliceD2_Lib.hi', 'TH_spliceD2_Lib.o'])


# Testing profiling with TH is a bit tricky; we've already disabled
# the prof way above, and also we want to add options specifically for
# profiling (-osof p_o) because this is necessary when mixing
# profiling w/ TH.  Furthermore we must have built the program the
# normal way first, so this is a bit of a hack.
test('TH_spliceE5_prof', normal, multimod_compile_and_run, ['TH_spliceE5', '-v0 -prof -auto-all -osuf p_o'])
clean(['TH_spliceE5_Lib.hi', 'TH_spliceE5_Lib.o'])

test('TH_reifyDecl1', normal, compile, [''])

test('TH_reifyType1', normal, compile, [''])
test('TH_reifyType2', normal, compile, [''])

test('TH_spliceDecl1', normal, compile, ['-v0'])
test('TH_spliceDecl2', normal, compile, ['-v0'])
test('TH_spliceDecl3', normal, multimod_compile, ['TH_spliceDecl3', '-v0'])
clean(['TH_spliceDecl3_Lib.hi', 'TH_spliceDecl3_Lib.o'])
test('TH_spliceDecl4', normal, multimod_compile, ['TH_spliceDecl4', '-v0'])
clean(['TH_spliceDecl4_Lib.hi', 'TH_spliceDecl4_Lib.o'])

test('TH_spliceE1', normal, compile_and_run, [''])
test('TH_spliceExpr1', normal, compile, ['-v0'])
test('TH_spliceE3', normal, compile, [''])
test('TH_spliceE4', normal, compile_and_run, [''])

test('TH_bracket1', normal, compile, [''])
test('TH_bracket2', normal, compile, [''])
test('TH_bracket3', normal, compile, [''])

test('TH_class1', normal, compile, [''])
test('TH_tuple1', normal, compile, [''])
test('TH_genEx', normal, multimod_compile, ['TH_genEx', '-v0'])

test('TH_where', normal, compile_and_run, [''])

test('TH_spliceInst', normal, compile, [''])

test('TH_fail', normal, compile_fail, [''])

test('TH_dupdecl', normal, compile_fail, [''])
test('TH_exn', normal, compile_fail, [''])

test('TH_recover', normal, compile_and_run, [''])
