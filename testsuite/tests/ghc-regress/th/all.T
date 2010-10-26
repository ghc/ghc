
# This test needs to come before the setTestOpts calls below, as we want
# to run it !if_compiler_profiled
test('T4255', unless_compiler_profiled(skip), compile_fail, ['-v0'])

def f(opts):
  opts.extra_hc_opts = '-XTemplateHaskell -package template-haskell'
  if (ghc_with_interpreter == 0):
	opts.skip = 1

setTestOpts(f)
setTestOpts(only_compiler_types(['ghc']))
setTestOpts(only_ways(['normal','ghci']))
setTestOpts(if_compiler_profiled(skip))

test('TH_mkName', normal, compile, ['-v0'])
test('TH_1tuple', normal, compile_fail, ['-v0'])

test('TH_repE1', normal, compile, [''])
test('TH_repE2', normal, compile_and_run, [''])
test('TH_repE3', normal, compile, [''])
test('TH_repPrim', normal, compile, ['-v0'])
test('TH_spliceGuard', normal, compile, ['-v0'])
test('TH_repPrimOutput', normal, compile_and_run, [''])
test('TH_repGuard', normal, compile, ['-v0'])
test('TH_repGuardOutput', normal, compile_and_run, [''])
test('TH_repPatSig', normal, compile_fail, [''])

test('TH_spliceE5',
     extra_clean(['TH_spliceE5_Lib.hi', 'TH_spliceE5_Lib.o']),
     multimod_compile_and_run,
     ['TH_spliceE5.hs', '-v0'])

test('TH_spliceE6', normal, compile, ['-v0'])

test('TH_NestedSplices',
     extra_clean(['TH_NestedSplices_Lib.hi', 'TH_NestedSplices_Lib.o']),
     multimod_compile,
     ['TH_NestedSplices.hs', '-v0'])

# Testing profiling with TH is a bit tricky; we've already disabled
# the prof way above, and also we want to add options specifically for
# profiling (-osuf p_o) because this is necessary when mixing
# profiling w/ TH.  Furthermore we must have built the program the
# normal way first, which is why the work is done by a Makefile rule.
test('TH_spliceE5_prof',
     [req_profiling,
      omit_ways(['ghci']),
      extra_clean(['TH_spliceE5_prof_Lib.p_o', 'TH_spliceE5_prof_Lib.hi',
                   'TH_spliceE5_prof_Lib.o','TH_spliceE5_prof.p_o'])],
     run_command,
     ['$MAKE -s --no-print-directory TH_spliceE5_prof'])

test('TH_spliceD1',
     extra_clean(['TH_spliceD1_Lib.hi', 'TH_spliceD1_Lib.o']),
     multimod_compile_fail,
     ['TH_spliceD1', '-v0'])

test('TH_spliceD2',
     extra_clean(['TH_spliceD2_Lib.hi', 'TH_spliceD2_Lib.o']),
     multimod_compile,
     ['TH_spliceD2', '-v0'])

test('TH_reifyDecl1', normal, compile, ['-v0'])
test('TH_reifyDecl2', normal, compile, ['-v0'])

test('TH_reifyType1', normal, compile, [''])
test('TH_reifyType2', normal, compile, [''])
test('TH_reifyMkName', normal, compile, ['-v0'])

test('TH_spliceDecl1', normal, compile, ['-v0'])
test('TH_spliceDecl2', normal, compile, ['-v0'])
test('TH_spliceDecl3',
     extra_clean(['TH_spliceDecl3_Lib.hi', 'TH_spliceDecl3_Lib.o']),
     multimod_compile,
     ['TH_spliceDecl3', '-v0'])
test('TH_spliceDecl4',
     extra_clean(['TH_spliceDecl4_Lib.hi', 'TH_spliceDecl4_Lib.o']),
     multimod_compile,
     ['TH_spliceDecl4', '-v0'])

test('T2597a',
     extra_clean(['T2597a_Lib.hi', 'T2597a_Lib.o']),
     multimod_compile,
     ['T2597a', '-v0'])

test('T2597b',
     extra_clean(['T2597b_Lib.hi', 'T2597b_Lib.o']),
     multimod_compile_fail,
     ['T2597b', '-v0'])

test('TH_spliceE1', normal, compile_and_run, [''])
test('TH_spliceExpr1', normal, compile, ['-v0'])
test('TH_spliceE3', normal, compile, ['-v0'])
test('TH_spliceE4', normal, compile_and_run, [''])

test('TH_bracket1', normal, compile, [''])
test('TH_bracket2', normal, compile, [''])
test('TH_bracket3', normal, compile, [''])

test('TH_class1', normal, compile, ['-v0'])
test('TH_tuple1', normal, compile, ['-v0'])
test('TH_genEx',
     [extra_clean(['TH_genExLib.hi', 'TH_genExLib.o']),
      if_compiler_lt('ghc', '6.11', expect_fail)],
     multimod_compile,
     ['TH_genEx', '-v0'])

test('TH_where', normal, compile_and_run, [''])

test('TH_spliceInst', normal, compile, ['-v0'])

test('TH_exn1', normal, compile_fail, ['-v0'])

test('TH_dupdecl', normal, compile_fail, ['-v0'])
test('TH_exn2', normal, compile_fail, ['-v0'])

test('TH_recover', normal, compile_and_run, [''])
test('TH_dataD1', normal, compile_fail, ['-v0'])

test('TH_ppr1', normal, compile_and_run, [''])

test('TH_fail', normal, compile_fail, ['-v0'])
test('TH_scopedTvs', normal, compile_fail, ['-v0'])

test('TH_runIO', normal, compile_fail, ['-v0'])

test('TH_ghci1', normal, ghci_script, ['TH_ghci1.script'])

test('TH_scope', normal, compile, [''])
test('T2632', normal, compile, [''])
test('T2700', normal, compile, ['-v0'])
test('T2817', normal, compile, ['-v0'])
test('T2713', normal, compile_fail, ['-v0'])
test('T2674', normal, compile_fail, ['-v0'])
test('T2931', normal, compile, ['-v0'])
test('TH_emptycase', normal, compile_fail, ['-v0'])

test('T2386', extra_clean(['T2386_Lib.hi', 'T2386_Lib.o']),
	      run_command,
	      ['$MAKE -s --no-print-directory T2386'] )

test('T2685', extra_clean(['T2685a.hi','T2685a.o']), 
              multimod_compile, ['T2685','-v0'])

test('TH_sections', normal, compile, ['-v0'])

test('TH_tf1', if_compiler_lt('ghc', '6.11', expect_fail), compile, ['-v0'])
test('TH_tf2', if_compiler_lt('ghc', '6.11', expect_fail), compile, ['-v0'])
test('TH_tf3', if_compiler_lt('ghc', '6.11', expect_fail), compile, ['-v0'])

test('TH_pragma',
     if_compiler_lt('ghc', '6.11', expect_fail),
     compile,
     ['-v0 -dsuppress-uniques'])
test('T3177', normal, compile, ['-v0'])
test('T3177a', normal, compile_fail, ['-v0'])

test('T3319', normal, compile, ['-ddump-splices -v0'])
test('TH_foreignInterruptible', normal, compile, ['-ddump-splices -v0'])

test('T3395', normal, compile_fail, ['-v0'])
test('T3467', normal, compile, [''])
test('T3572', normal, compile_and_run, [''])
test('T3100', normal, compile, ['-v0'])
test('T3920', normal, compile_and_run, ['-v0'])

test('T3600', extra_clean(['T3600a.hi','T3600a.o']), 
              multimod_compile, ['T3600','-v0'])
test('T3845', normal, compile, ['-v0'])
test('T3899', extra_clean(['T3899a.hi','T3899a.o']), 
              multimod_compile, ['T3899','-v0 -ddump-splices -dsuppress-uniques'])
test('T4056', normal, compile, ['-v0'])
test('T4188', normal, compile, ['-v0'])
test('T4233', normal, compile, ['-v0'])
test('T4169', normal, compile, ['-v0'])
test('T1835', normal, compile_and_run, ['-v0'])

test('TH_viewPatPrint', normal, compile_and_run, [''])
test('T4436', normal, compile, ['-v0 -ddump-splices'])
