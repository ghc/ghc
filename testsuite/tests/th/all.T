# NOTICE TO DEVELOPERS
# ~~~~~~~~~~~~~~~~~~~~
# Adding a TemplateHaskell test?  If it only contains (non-quasi) quotes
# and no splices, consider adding it to the quotes/ directory instead
# of the th/ directory; this way, we can test it on the stage 1 compiler too!

def f(name, opts):
    opts.extra_hc_opts = '-XTemplateHaskell -package template-haskell'
    if (ghc_with_interpreter == 0):
        opts.skip = 1

setTestOpts(f)
setTestOpts(only_ways(['normal','ghci']))

test('TH_mkName', normal, compile, ['-v0'])
test('TH_1tuple', normal, compile_fail, ['-v0'])

test('TH_repE2', normal, compile_and_run, [''])
test('TH_repPrim', normal, compile, ['-v0'])
test('TH_repPrim2', normal, compile, ['-v0'])
test('TH_repUnboxedTuples', normal, compile, ['-v0'])
test('TH_spliceGuard', normal, compile, ['-v0'])
test('TH_repPrimOutput', normal, compile_and_run, [''])
test('TH_repPrimOutput2', normal, compile_and_run, [''])
test('TH_repGuard', normal, compile, ['-v0'])
test('TH_repGuardOutput', normal, compile_and_run, [''])
test('TH_repPatSig', normal, compile_fail, [''])

test('TH_spliceE5',
     extra_clean(['TH_spliceE5_Lib.hi', 'TH_spliceE5_Lib.o']),
     multimod_compile_and_run,
     ['TH_spliceE5.hs', '-v0 ' + config.ghc_th_way_flags])

test('TH_spliceE6', normal, compile, ['-v0'])

test('TH_NestedSplices',
     extra_clean(['TH_NestedSplices_Lib.hi', 'TH_NestedSplices_Lib.o']),
     multimod_compile,
     ['TH_NestedSplices.hs', '-v0 ' + config.ghc_th_way_flags])

# Testing profiling with TH is a bit tricky; we've already disabled
# the prof way above, and also we want to add options specifically for
# profiling (-osuf p.o) because this is necessary when mixing
# profiling w/ TH.  Furthermore we must have built the program the
# normal way first, which is why the work is done by a Makefile rule.
test('TH_spliceE5_prof',
     [req_profiling,
      omit_ways(['ghci']),
      extra_clean(['TH_spliceE5_prof_Lib.p.o', 'TH_spliceE5_prof_Lib.hi',
                   'TH_spliceE5_prof_Lib.o','TH_spliceE5_prof.p.o'])],
     run_command,
     ['$MAKE -s --no-print-directory TH_spliceE5_prof'])

test('TH_spliceE5_prof_ext',
     [req_profiling,
      omit_ways(['ghci']),
      extra_clean(['TH_spliceE5_prof_ext_Lib.hi',
                   'TH_spliceE5_prof_ext_Lib.o'])],
     run_command,
     ['$MAKE -s --no-print-directory TH_spliceE5_prof_ext'])

test('TH_spliceD1',
     extra_clean(['TH_spliceD1_Lib.hi', 'TH_spliceD1_Lib.o']),
     multimod_compile_fail,
     ['TH_spliceD1', '-v0 ' + config.ghc_th_way_flags])

test('TH_spliceD2',
     extra_clean(['TH_spliceD2_Lib.hi', 'TH_spliceD2_Lib.o']),
     multimod_compile,
     ['TH_spliceD2', '-v0'])

test('TH_reifyDecl1', normal, compile, ['-v0'])
test('TH_reifyDecl2', normal, compile, ['-v0'])

test('TH_reifyMkName', normal, compile, ['-v0'])

test('TH_reifyInstances', normal, compile, ['-v0'])

test('TH_spliceDecl1', normal, compile, ['-v0'])
test('TH_spliceDecl2', normal, compile, ['-v0'])
test('TH_spliceDecl3',
     extra_clean(['TH_spliceDecl3_Lib.hi', 'TH_spliceDecl3_Lib.o']),
     multimod_compile,
     ['TH_spliceDecl3', '-v0 ' + config.ghc_th_way_flags])
test('TH_spliceDecl4',
     extra_clean(['TH_spliceDecl4_Lib.hi', 'TH_spliceDecl4_Lib.o']),
     multimod_compile,
     ['TH_spliceDecl4', '-v0 ' + config.ghc_th_way_flags])

test('T2597a',
     extra_clean(['T2597a_Lib.hi', 'T2597a_Lib.o']),
     multimod_compile,
     ['T2597a', '-v0 ' + config.ghc_th_way_flags])

test('T2597b',
     extra_clean(['T2597b_Lib.hi', 'T2597b_Lib.o']),
     multimod_compile_fail,
     ['T2597b', '-v0 ' + config.ghc_th_way_flags])

test('TH_spliceE1', normal, compile_and_run, [''])
test('TH_spliceExpr1', normal, compile, ['-v0'])
test('TH_spliceE3', normal, compile, ['-v0'])
test('TH_spliceE4', normal, compile_and_run, [''])

test('TH_class1', normal, compile, ['-v0'])
test('TH_tuple1', normal, compile, ['-v0'])
test('TH_genEx',
     extra_clean(['TH_genExLib.hi', 'TH_genExLib.o']),
     multimod_compile,
     ['TH_genEx', '-v0 ' + config.ghc_th_way_flags])

test('TH_where', normal, compile_and_run, [''])

test('TH_spliceInst', normal, compile, ['-v0'])

test('TH_exn1', normal, compile_fail, ['-v0'])

test('TH_dupdecl', normal, compile_fail, ['-v0'])
test('TH_exn2', normal, compile_fail, ['-v0'])

test('TH_recover', normal, compile_and_run, [''])
test('TH_dataD1', normal, compile_fail, ['-v0'])

test('TH_fail', normal, compile_fail, ['-v0'])
test('TH_scopedTvs', normal, compile, ['-v0'])

test('TH_runIO', normal, compile_fail, ['-v0'])

test('TH_ghci1', normal, ghci_script, ['TH_ghci1.script'])

test('TH_linePragma', normal, compile_fail, ['-v0'])

test('T1830_3',
     extra_clean(['T1830_3a.o','T1830_3a.hi']),
     multimod_compile_and_run,
     ['T1830_3', '-v0 ' + config.ghc_th_way_flags])
test('T2700', normal, compile, ['-v0'])
test('T2817', normal, compile, ['-v0'])
test('T2713', normal, compile_fail, ['-v0'])
test('T2674', normal, compile_fail, ['-v0'])
test('TH_emptycase', normal, compile, ['-v0'])

test('T2386', extra_clean(['T2386_Lib.hi', 'T2386_Lib.o']),
	      run_command,
	      ['$MAKE -s --no-print-directory T2386'] )

test('T2685', extra_clean(['T2685a.hi','T2685a.o']),
              multimod_compile, ['T2685', '-v0 ' + config.ghc_th_way_flags])

test('TH_sections', normal, compile, ['-v0'])

test('TH_tf1', normal, compile, ['-v0'])
test('TH_tf3', normal, compile, ['-v0'])

test('TH_pragma', normal, compile, ['-v0 -dsuppress-uniques'])
test('T3177', normal, compile, ['-v0'])
test('T3177a', normal, compile_fail, ['-v0'])

test('T3319', normal, compile, ['-ddump-splices -v0'])
test('TH_foreignInterruptible', normal, compile, ['-ddump-splices -v0'])
test('TH_foreignCallingConventions', normal,
                                     compile,
                                     ['-ddump-splices -dsuppress-uniques -v0'])

test('T3395', normal, compile_fail, ['-v0'])
test('T3467', normal, compile, [''])
test('T3100', normal, compile, ['-v0'])
test('T3920', normal, compile_and_run, ['-v0'])

test('T3600', extra_clean(['T3600a.hi','T3600a.o']),
              multimod_compile, ['T3600', '-v0 ' + config.ghc_th_way_flags])
test('T3845', normal, compile, ['-v0'])
test('T3899', extra_clean(['T3899a.hi','T3899a.o']),
              multimod_compile,
              ['T3899','-v0 -ddump-splices -dsuppress-uniques ' + config.ghc_th_way_flags])
test('T4188', normal, compile, ['-v0'])
test('T4233', normal, compile, ['-v0'])
test('T1835', normal, compile_and_run, ['-v0'])

test('TH_viewPatPrint', normal, compile_and_run, [''])
test('T4436', normal, compile, ['-v0 -ddump-splices'])
test('T4949', normal, compile, ['-v0'])
test('T5126', normal, compile, ['-v0'])
test('T5217', normal, compile, ['-v0 -dsuppress-uniques -ddump-splices'])
test('T5037', normal, compile, ['-v0'])
test('TH_unboxedSingleton', normal, compile, ['-v0'])
test('T5290', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T5362', normal, compile, ['-v0'])

test('TH_unresolvedInfix',
     extra_clean(['TH_unresolvedInfix_Lib.hi', 'TH_unresolvedInfix_Lib.o']),
     multimod_compile_and_run,
     ['TH_unresolvedInfix.hs', '-v0 ' + config.ghc_th_way_flags])
test('TH_unresolvedInfix2',
     normal,
     compile_fail,
     ['-v0'])

test('T5358', normal, compile_fail, [' -v0'])
test('T5379', normal, compile_and_run, [''])
test('T5404', normal, compile, ['-v0'])
test('T5410', normal, compile_and_run, ['-v0'])
test('TH_lookupName',
     extra_clean(['TH_lookupName_Lib.hi', 'TH_lookupName_Lib.o']),
     multimod_compile_and_run,
     ['TH_lookupName.hs', config.ghc_th_way_flags])
test('T5452', normal, compile, ['-v0'])
test('T5434', extra_clean(['T5434a.hi','T5434a.o']),
              multimod_compile,
              ['T5434','-v0 -Wall ' + config.ghc_th_way_flags])
test('T5508', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_Depends',
     [extra_clean(['TH_Depends_External.o', 'TH_Depends_External.hi',
                   'TH_Depends_external.txt'])],
     run_command,
     ['$MAKE -s --no-print-directory TH_Depends'])
test('T5597', extra_clean(['T5597a.hi','T5597a.o']),
              multimod_compile, ['T5597', '-v0 ' + config.ghc_th_way_flags])
test('T5665', extra_clean(['T5665a.hi','T5665a.o']),
              multimod_compile, ['T5665', '-v0 ' + config.ghc_th_way_flags])
test('T5700', extra_clean(['T5700a.hi','T5700a.o']),
              multimod_compile,
              ['T5700','-v0 -ddump-splices ' + config.ghc_th_way_flags])

test('TH_PromotedTuple', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_PromotedList', normal, compile, ['-v0'])
test('TH_Promoted1Tuple', normal, compile_fail, ['-v0'])
test('TH_RichKinds', normal, compile, ['-v0'])
test('TH_RichKinds2', normal, compile, ['-v0'])

test('T1541', normal, compile, ['-v0'])
test('T5883', normal, compile, ['-v0 -dsuppress-uniques -ddump-splices'])
test('T5882', normal, compile, ['-v0'])
test('T5886', extra_clean(['T5886a.hi','T5886a.o']),
              multimod_compile, ['T5886','-v0 ' + config.ghc_th_way_flags])
test('T4135',  normal, compile, ['-v0'])
test('T4135a', normal, compile, ['-v0'])
test('T5971', normal, compile_fail, ['-v0 -dsuppress-uniques'])
test('T5968', normal, compile, ['-v0'])
test('T5984', extra_clean(['T5984_Lib.hi', 'T5984_Lib.o']),
              multimod_compile,
              ['T5984', '-v0 -ddump-splices ' + config.ghc_th_way_flags])
test('T5555', extra_clean(['T5555_Lib.hi', 'T5555_Lib.o']),
              multimod_compile, ['T5555', '-v0 ' + config.ghc_th_way_flags])
test('T5976', normal, compile_fail, ['-v0'])
test('T5795', normal, compile_fail, ['-v0'])
test('T6005', normal, compile, ['-v0'])
test('T6005a', normal, compile, ['-v0'])
test('T5737', normal, compile, ['-v0'])
test('T6114', normal, compile, ['-v0'])
test('TH_StringPrimL', normal, compile_and_run, [''])
test('T7064',
     extra_clean(['T7064a.hi', 'T7064a.o']),
     multimod_compile_and_run,
     ['T7064.hs', '-v0 ' + config.ghc_th_way_flags])
test('T7092', extra_clean(['T7092a.hi','T7092a.o']),
              multimod_compile, ['T7092', '-v0 ' + config.ghc_th_way_flags])
test('T7276', normal, compile_fail, ['-v0'])
test('T7276a', combined_output, ghci_script, ['T7276a.script'])

test('TH_TyInstWhere1', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_TyInstWhere2', normal, compile, ['-v0'])

test('T7445', extra_clean(['T7445a.hi', 'T7445a.o']),
	      run_command,
	      ['$MAKE -s --no-print-directory T7445'] )
test('T7532',
     extra_clean(['T7532a.hi', 'T7532a.o']),
     multimod_compile,
     ['T7532', '-v0 ' + config.ghc_th_way_flags])
test('T2222', normal, compile, ['-v0'])
test('T1849', normal, ghci_script, ['T1849.script'])
test('T7681', normal, compile, ['-v0'])
test('T7910', normal, compile_and_run, ['-v0'])

test('ClosedFam1TH', normal, compile, ['-dsuppress-uniques -v0'])
test('ClosedFam2TH', normal, compile, ['-v0'])

test('T8028',
     extra_clean(['T8028a.hi', 'T8028a.o']),
     multimod_compile,
     ['T8028', '-v0 ' + config.ghc_th_way_flags])

test('TH_Roles1', normal, compile_fail, ['-v0'])
test('TH_Roles2', normalise_version('array', 'base', 'deepseq', 'ghc-prim',
                                    'integer-gmp', 'pretty', 'template-haskell'
                                    ), compile, ['-v0 -ddump-tc'])
test('TH_Roles3', normal, compile, ['-v0 -dsuppress-uniques'])
test('TH_Roles4', normal, compile, ['-v0'])

test('T8186', normal, compile_and_run, ['-v0'])

test('T8333',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T8333'])

test('T4124', normal, compile, ['-v0'])
test('T4128', normal, compile, ['-v0'])
test('T4364', normal, compile, ['-v0'])
test('T8412', normal, compile_fail, ['-v0'])
test('T7667', normal, compile, ['-v0'])
test('T7667a', normal, compile_fail, ['-v0'])
test('T8499', normal, compile, ['-v0'])
test('T7477', normal, compile, ['-v0'])
test('T8507', normal, compile, ['-v0'])
test('T8540',
     extra_clean(['T8540a.hi', 'T8540a.o']),
     multimod_compile,
     ['T8540', '-v0 ' + config.ghc_th_way_flags])
test('T8577',
     extra_clean(['T8577a.hi', 'T8577a.o']),
     multimod_compile_fail,
     ['T8577', '-v0 ' + config.ghc_th_way_flags])
test('T8625', normal, ghci_script, ['T8625.script'])
test('TH_StaticPointers',
     [ when(compiler_lt('ghc', '7.9'), skip)
     , when(doing_ghci(), extra_hc_opts('-fobject-code'))
     ],
     compile_and_run, [''])
test('TH_StaticPointers02',
     [ when(compiler_lt('ghc', '7.9'), skip) ],
     compile_fail, [''])
test('T8759', normal, compile_fail, ['-v0'])
test('T7021',
     extra_clean(['T7021a.hi', 'T7021a.o']), multimod_compile,
     ['T7021','-v0 ' + config.ghc_th_way_flags])
test('T8807', normal, compile, ['-v0'])
test('T8884', normal, compile, ['-v0'])
test('T8954', normal, compile, ['-v0'])
test('T8932', normal, compile_fail, ['-v0'])
test('T8987', normal, compile_fail, ['-v0'])
test('T7241', normal, compile_fail, ['-v0'])
test('T9262', normal, compile, ['-v0'])
test('T9199', normal, compile, ['-v0'])
test('T9692', normal, compile, ['-v0'])
test('T8953', normal, compile, ['-v0'])
test('T9084', normal, compile_fail, ['-v0'])
test('T9738', normal, compile, ['-v0'])
test('T9081', normal, compile, ['-v0'])
test('T9066', normal, compile, ['-v0'])
test('T8100', normal, compile, ['-v0'])
test('T9064', normal, compile, ['-v0'])
test('T9209', normal, compile_fail, ['-v0'])
test('T7484', normal, compile_fail, ['-v0'])
test('T1476', normal, compile, ['-v0'])
test('T1476b', normal, compile, ['-v0'])
test('T8031', normal, compile, ['-v0'])
test('T8624', normal, run_command, ['$MAKE -s --no-print-directory T8624'])
test('TH_Lift', normal, compile, ['-v0'])
test('T10047', normal, ghci_script, ['T10047.script'])
test('T10019', normal, ghci_script, ['T10019.script'])
test('T10267', extra_clean(['T10267a.hi', 'T10267a.o']),
               multimod_compile_fail,
               ['T10267', '-dsuppress-uniques -v0 ' + config.ghc_th_way_flags])
test('T10279', normal, compile_fail, ['-v0'])
test('T10306', normal, compile, ['-v0'])
test('T10596', normal, compile, ['-v0'])
test('T10620', normal, compile_and_run, ['-v0'])
test('T10638', normal, compile_fail, ['-v0'])
test('T10704',
     extra_clean(['T10704a.o','T10704a.hi']),
     multimod_compile_and_run,
     ['T10704', '-v0 ' + config.ghc_th_way_flags])
test('T6018th', normal, compile_fail, ['-v0'])
test('TH_namePackage', normal, compile_and_run, ['-v0'])
test('TH_nameSpace', normal, compile_and_run, ['-v0'])
test('T10796a', normal, compile, ['-v0'])
test('T10796b', normal, compile_fail, ['-v0'])
test('T10811', normal, compile, ['-v0'])
test('T10810', normal, compile, ['-v0'])
test('T10828', normalise_version('array', 'base', 'binary', 'bytestring',
                                 'containers', 'deepseq', 'ghc-boot',
                                 'ghc-prim', 'integer-gmp', 'pretty',
                                 'template-haskell'
                              ), compile, ['-v0 -ddump-tc -dsuppress-uniques'])
test('T10828a', normal, compile_fail, ['-v0'])
test('T10828b', normal, compile_fail, ['-v0'])
test('T10891', normal, compile, ['-v0'])
test('T10945', normal, compile_fail, ['-v0'])
test('T10946', expect_broken(10946), compile, ['-v0'])
test('T10734', normal, compile_and_run, ['-v0'])
test('T10819',
     extra_clean(['T10819_Lib.hi', 'T10819_Lib.o']),
     multimod_compile,
     ['T10819.hs', '-v0 ' + config.ghc_th_way_flags])
test('T10820', normal, compile_and_run, ['-v0'])

test('TH_finalizer', normal, compile, ['-v0'])
