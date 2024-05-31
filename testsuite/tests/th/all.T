# NOTICE TO DEVELOPERS
# ~~~~~~~~~~~~~~~~~~~~
# Adding a TemplateHaskell test?  If it only contains (non-quasi) quotes
# and no splices, consider adding it to the quotes/ directory instead
# of the th/ directory; this way, we can test it on the stage 1 compiler too!

def f(name, opts):
    opts.extra_hc_opts = '-XTemplateHaskell -package template-haskell'
setTestOpts(f)
setTestOpts(req_th)

# TH should work with -fexternal-interpreter too
if config.have_ext_interp :
     setTestOpts(extra_ways(['ext-interp']))
     setTestOpts(only_ways(['normal','ext-interp'] + ghci_ways))

     if llvm_build():
         setTestOpts(fragile_for(16087, ['ext-interp']))

test('TH_mkName', normal, compile, ['-v0'])
test('TH_overloadedlabels', normal, compile, ['-v0'])
test('T20185', extra_files(['T20185a.hs']), multimod_compile_and_run, ['T20185', '-v0 ' + config.ghc_th_way_flags])
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
test('TH_repPatSig', [], multimod_compile,
     ['TH_repPatSig.hs', '-v0 ' + config.ghc_th_way_flags])
test('TH_repPatSigTVar', normal, compile_fail, ['-v0'])

test('TH_overlaps', normal, compile, ['-v0'])

test('TH_spliceE5', [], multimod_compile_and_run,
     ['TH_spliceE5.hs', '-v0 ' + config.ghc_th_way_flags])

test('TH_spliceE6', normal, compile, ['-v0'])

test('TH_NestedSplices', [], multimod_compile,
     ['TH_NestedSplices.hs', '-v0 ' + config.ghc_th_way_flags])
test('TH_NestedSplicesFail1', normal, compile_fail, ['-v0'])
test('TH_NestedSplicesFail2', normal, compile_fail, ['-v0'])
test('TH_NestedSplicesFail3', normal, compile_fail, ['-v0'])
test('TH_NestedSplicesFail4', normal, compile_fail, ['-v0'])
test('TH_NestedSplicesFail5', normal, compile_fail, ['-v0'])
test('TH_NestedSplicesFail6', normal, compile_fail, ['-v0'])
test('TH_NestedSplicesFail7', normal, compile_fail, ['-v0'])
test('TH_NestedSplicesFail8', normal, compile_fail, ['-v0'])

# Testing profiling with TH is a bit tricky (when not using
# -fexternal-interpreter); we've already disabled
# the prof way above, and also we want to add options specifically for
# profiling (-osuf p.o) because this is necessary when mixing
# profiling w/ TH.  Furthermore we must have built the program the
# normal way first, which is why the work is done by a Makefile rule.
test('TH_spliceE5_prof',
     [req_profiling, only_ways(['normal']),
      when(ghc_dynamic(), expect_broken(11495))],
     makefile_test, ['TH_spliceE5_prof'])

test('TH_spliceE5_prof_ext', [req_profiling, req_rts_linker, only_ways(['normal'])],
     makefile_test, ['TH_spliceE5_prof_ext'])

test('TH_spliceD1', [], multimod_compile_fail,
     ['TH_spliceD1', '-v0 ' + config.ghc_th_way_flags])

test('TH_spliceD2', [], multimod_compile, ['TH_spliceD2', '-v0'])

test('TH_reifyDecl1', normal, compile, ['-v0'])
test('TH_reifyDecl2', normal, compile, ['-v0'])
test('TH_reifyLinear', normal, compile, ['-v0'])
test('TH_reifyLocalDefs', normal, compile, ['-v0'])
test('TH_reifyLocalDefs2', normal, compile, ['-v0'])


# Checks if the error output of an appended command has the given pattern.
def error_has(pattern):
    swap12 = '3>&1 1>&2 2>&3 3>&-' # Swap file descriptors 1 and 2.
    # pipefail: make a pipe fail if any of the commands fails
    # the following fails only if both the command fails and the pattern is found
    return('bash -o pipefail -c \'! (! "$@" {swap12}) | grep {pattern} {swap12} &> /dev/null\' --'.format(**locals()))

test('TH_reifyMkName', normal, compile, ['-v0'])

test('TH_reifyInstances', normal, compile, ['-v0'])

test('TH_reifyExplicitForAllFams', normal, compile, ['-v0'])
test('TH_ExplicitForAllRules', normal, multimod_compile_and_run,
     ['TH_ExplicitForAllRules.hs', '-v0 ' + config.ghc_th_way_flags])

test('TH_spliceDecl1', normal, compile, ['-v0'])
test('TH_spliceDecl2', normal, compile, ['-v0'])
test('TH_spliceDecl3', [], multimod_compile,
     ['TH_spliceDecl3', '-v0 ' + config.ghc_th_way_flags])
test('TH_spliceDecl4', [], multimod_compile,
     ['TH_spliceDecl4', '-v0 ' + config.ghc_th_way_flags])

test('T2597a', [], multimod_compile,
     ['T2597a', '-v0 ' + config.ghc_th_way_flags])

test('T2597b', [], multimod_compile_fail,
     ['T2597b', '-v0 ' + config.ghc_th_way_flags])

test('TH_spliceE1', normal, compile_and_run, [''])
test('TH_spliceExpr1', normal, compile, ['-v0'])
test('TH_spliceE3', normal, compile, ['-v0'])
test('TH_spliceE4', normal, compile_and_run, [''])

test('TH_class1', normal, compile, ['-v0'])
test('TH_tuple1', [], multimod_compile_and_run,
    ['TH_tuple1', '-v0 ' + config.ghc_th_way_flags])
test('TH_genEx', [], multimod_compile,
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

test('TH_ghci1', only_ways(['ghci']), ghci_script, ['TH_ghci1.script'])

test('TH_linePragma', normal, compile_fail, ['-v0'])

test('T1830_3', [], multimod_compile_and_run,
     ['T1830_3', '-v0 ' + config.ghc_th_way_flags])
test('T2700', normal, compile, ['-v0'])
test('T2817', normal, compile, ['-v0'])
test('T2713', normal, compile_fail, ['-v0'])
test('T2674', normal, compile_fail, ['-v0'])
test('TH_emptycase', normal, compile, ['-v0'])
test('T24046', normal, compile, ['-v0'])

test('T2386', [only_ways(['normal'])], makefile_test, ['T2386'])

test('T2685', [], multimod_compile, ['T2685', '-v0 ' + config.ghc_th_way_flags])

test('TH_sections', normal, compile, ['-v0'])

test('TH_tf1', normal, compile, ['-v0'])
test('TH_tf3', normal, compile, ['-v0'])

test('TH_pragma', normal, compile, ['-v0 -dsuppress-uniques'])
test('T24074', normal, compile, ['-v0 -dsuppress-uniques -ddump-splices'])
test('T3177', normal, compile, ['-v0'])
test('T3177a', normal, compile_fail, ['-v0'])

test('T3319', normal, compile, ['-ddump-splices -v0'])
test('TH_foreignInterruptible',
     when(unregisterised(), fragile(16085)),
     compile, ['-ddump-splices -v0'])
test('TH_foreignCallingConventions', normal,
                                     compile,
                                     ['-ddump-splices -dsuppress-uniques -v0'])

test('T3395', normal, compile_fail, ['-v0'])
test('T3467', normal, compile, [''])
test('T3100', normal, compile, ['-v0'])
test('T3920', normal, compile_and_run, ['-v0'])

test('T3600', [], multimod_compile, ['T3600', '-v0 ' + config.ghc_th_way_flags])
test('T3845', normal, compile, ['-v0'])
test('T3899', [], multimod_compile,
     ['T3899',
      '-v0 -ddump-splices -dsuppress-uniques ' + config.ghc_th_way_flags])
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

test('TH_unresolvedInfix', [], multimod_compile_and_run,
     ['TH_unresolvedInfix.hs', '-v0 ' + config.ghc_th_way_flags])
test('TH_unresolvedInfix2',
     normal,
     compile_fail,
     ['-v0'])

test('T5358', normal, compile_fail, [' -v0'])
test('T5379', normal, compile_and_run, [''])
test('T5404', normal, compile, ['-v0'])
test('T5410', normal, compile_and_run, ['-v0'])
test('TH_lookupName', [], multimod_compile_and_run,
     ['TH_lookupName.hs', config.ghc_th_way_flags])
test('T5452', normal, compile, ['-v0'])
test('T5434', [], multimod_compile,
     ['T5434', '-v0 -Wall ' + config.ghc_th_way_flags])
test('T5508', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_Depends', [only_ways(['normal'])], makefile_test, ['TH_Depends'])
test('T5597', [], multimod_compile, ['T5597', '-v0 ' + config.ghc_th_way_flags])
test('T5665', [], multimod_compile, ['T5665', '-v0 ' + config.ghc_th_way_flags])
test('T5700', [], multimod_compile,
     ['T5700', '-v0 -ddump-splices ' + config.ghc_th_way_flags])

test('TH_PromotedTuple', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_PromotedList', normal, compile, ['-v0'])
test('TH_Promoted1Tuple', normal, compile_fail, ['-v0'])
test('TH_RichKinds', normal, compile, ['-v0'])
test('TH_RichKinds2', normal, compile, ['-v0'])

test('T1541', normal, compile, ['-v0'])
test('T5883', normal, compile, ['-v0 -dsuppress-uniques -ddump-splices'])
test('T5882', normal, compile, ['-v0'])
test('T5886', [], multimod_compile, ['T5886', '-v0 ' + config.ghc_th_way_flags])
test('T4135',  normal, compile, ['-v0'])
test('T4135a', normal, compile, ['-v0'])
test('T5971', normal, compile_fail, ['-v0 -dsuppress-uniques'])
test('T5968', normal, compile, ['-v0'])
test('T5984', [], multimod_compile,
     ['T5984', '-v0 -ddump-splices ' + config.ghc_th_way_flags])
test('T5555', [], multimod_compile, ['T5555', '-v0 ' + config.ghc_th_way_flags])
test('T5976', normal, compile_fail, ['-v0'])
test('T5795', normal, compile_fail, ['-v0'])
test('T6005', normal, compile, ['-v0'])
test('T6005a', normal, compile, ['-v0'])
test('T5737', normal, compile, ['-v0'])
test('T6114', normal, compile, ['-v0'])
test('TH_StringPrimL', normal, compile_and_run, [''])
test('T7064', [], multimod_compile_and_run,
     ['T7064.hs', '-v0 ' + config.ghc_th_way_flags])
test('T7092', [], multimod_compile, ['T7092', '-v0 ' + config.ghc_th_way_flags])
test('T7276', normal, compile_fail, ['-v0'])
test('T7276a', [ only_ways(['ghci']), combined_output ],
               ghci_script, ['T7276a.script'])

test('TH_TyInstWhere1', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_TyInstWhere2', normal, compile, ['-v0'])

test('T7445', [only_ways(['normal'])], makefile_test, ['T7445'])
test('T7532', [], multimod_compile, ['T7532', '-v0 ' + config.ghc_th_way_flags])
test('T2222', normal, compile, ['-v0'])
test('T1849', only_ways(['ghci']), ghci_script, ['T1849.script'])
test('T7681', normal, compile, ['-v0'])
test('T7910', normal, compile_and_run, ['-v0'])

test('ClosedFam1TH', normal, compile, ['-dsuppress-uniques -v0'])
test('ClosedFam2TH', normal, compile, ['-v0'])

test('T8028', [], multimod_compile, ['T8028', '-v0 ' + config.ghc_th_way_flags])

test('TH_Roles1', normal, compile_fail, ['-v0'])
test('TH_Roles2', normalise_version('ghc-internal', 'array', 'base', 'deepseq', 'ghc-prim',
                                    'ghc-boot', 'ghc-boot-th',
                                    'ghc-bignum', 'pretty', 'template-haskell',
                                    'binary', 'bytestring', 'containers'
                                    ), compile, ['-v0 -ddump-tc -dsuppress-uniques'])
test('TH_Roles3', normal, compile, ['-v0 -dsuppress-uniques'])
test('TH_Roles4', normal, compile, ['-v0'])

test('T8186', normal, compile_and_run, ['-v0'])

test('T8333',
     [req_interp, only_ways(['normal'])],
     makefile_test, ['T8333'])

test('T4124', normal, compile, ['-v0'])
test('T4128', normal, compile, ['-v0'])
test('T4364', normal, compile, ['-v0'])
test('T8412', normal, compile_fail, ['-v0'])
test('T7667', normal, compile, ['-v0'])
test('T7667a', normal, compile_fail, ['-v0'])
test('T8499', normal, compile, ['-v0'])
test('T7477', normal, compile, ['-v0'])
test('T8507', normal, compile, ['-v0'])
test('T8540', [], multimod_compile, ['T8540', '-v0 ' + config.ghc_th_way_flags])
test('T8577', [], multimod_compile_fail,
     ['T8577', '-v0 ' + config.ghc_th_way_flags])
test('T8625', only_ways(['ghci']), ghci_script, ['T8625.script'])
test('TH_StaticPointers', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, [''])
test('TH_StaticPointers02', [], compile_fail, [''])
test('T8759', normal, compile, ['-v0'])
test('T7021', [], multimod_compile, ['T7021', '-v0 ' + config.ghc_th_way_flags])
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
test('T8624', only_ways(['normal']), makefile_test, ['T8624'])
test('TH_Lift', normal, compile, ['-v0'])
test('T10047', only_ways(['ghci']), ghci_script, ['T10047.script'])
test('T10019', only_ways(['ghci']), ghci_script, ['T10019.script'])
test('T10267', [], multimod_compile_fail,
     ['T10267', '-fno-max-valid-hole-fits -dsuppress-uniques -v0 ' + config.ghc_th_way_flags])
test('T10279', normal, compile_fail, ['-v0'])
test('T10306', normal, compile, ['-v0'])
test('T10596', normal, compile, ['-v0'])
test('T10598_TH', normal, compile, ['-v0 -dsuppress-uniques -ddump-splices'])
test('T10620', normal, compile_and_run, ['-v0'])
test('T10638', normal, compile_fail, ['-v0'])
test('T10697_decided_1', normal, compile_and_run, ['-v0'])
test('T10697_decided_2', normal, compile_and_run, ['-XStrictData -v0'])
test('T10697_decided_3', normal, compile_and_run, ['-XStrictData -funbox-strict-fields -O2 -v0'])
test('T10697_source', [], multimod_compile_and_run,
     ['T10697_source', '-w ' + config.ghc_th_way_flags])
test('T10704', [], multimod_compile_and_run,
     ['T10704', '-v0 ' + config.ghc_th_way_flags])
test('T6018th', normal, compile_fail, ['-v0'])
test('TH_namePackage', normal, compile_and_run, ['-v0'])
test('TH_nameSpace', normal, compile_and_run, ['-v0'])
test('T10796a', normal, compile, ['-v0 -package containers'])
test('T10796b', normal, compile_fail, ['-v0 -package containers'])
test('T10811', normal, compile, ['-v0'])
test('T10810', normal, compile, ['-v0'])
test('T10828', normal, compile, ['-v0 -dsuppress-uniques'])
test('T10828a', normal, compile_fail, ['-v0'])
test('T10828b', normal, compile_fail, ['-v0'])
test('T10891', normal, compile, ['-v0'])
test('T10945', normal, compile_fail, ['-v0'])
test('T10946', normal, compile_fail, ['-v0'])
test('T10734', normal, compile_and_run, ['-v0'])
test('T10819', [], multimod_compile,
     ['T10819.hs', '-v0 ' + config.ghc_th_way_flags])
test('T10820', normal, compile_and_run, ['-v0'])
test('T11341', normal, compile, ['-v0 -dsuppress-uniques'])
test('T11342b', normal, compile, ['-v0'])
test('T11345', normal, compile_and_run, ['-v0 -dsuppress-uniques'])
test('TH_finalizer', normal, compile, ['-v0'])
test('TH_finalizer2',
     normal, multimod_compile_and_run,
     ['TH_finalizer2', '-v0 ' + config.ghc_th_way_flags])
test('T10603', normal, compile, ['-ddump-splices -dsuppress-uniques'])
test('T11452', normal, compile_fail, ['-v0'])
test('T9022', normal, compile_and_run, ['-v0'])
test('T11145', normal, compile_fail, ['-v0 -dsuppress-uniques'])
test('T11463', normal, compile_and_run, ['-v0 -dsuppress-uniques'])
test('T11680', normal, compile_fail, ['-v0'])
test('T11721_TH', normal, compile, ['-v0'])
test('T11809', normal, compile, ['-v0'])
test('T11797', normal, compile, ['-v0 -dsuppress-uniques'])
test('T11941', normal, compile_fail, ['-v0'])
test('T11484', normal, compile, ['-v0'])
test('T11629', normal, compile, ['-v0'])

test('T8761', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T12045TH1', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T12045TH2', normal, compile, ['-v0'])
test('T12073', normal, compile_and_run, [''])
test('T12130', [], multimod_compile,
     ['T12130', '-v0 ' + config.ghc_th_way_flags])
test('T12387', normal, compile_fail, ['-v0'])
test('T12387a', normal, compile_fail, ['-v0'])
test('T12403', normal,
              compile_and_run, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T12407', normal, compile, ['-v0'])
test('T12411', normal, compile_fail, [''])
test('T12478_1', normal, compile_and_run, ['-v0 -dsuppress-uniques'])
test('T12478_2', normal, compile_and_run, ['-v0'])
test('T12478_3', normal, compile, ['-v0'])
test('T12478_4', normal, compile_fail, ['-v0'])
test('T12478_5', normal, compile, ['-v0'])
test('T12503', normal, compile, ['-v0'])
test('T12513', normal, compile_fail, ['-v0'])
test('T12530', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T12646', normal, compile, ['-v0'])
test('T12788', [], multimod_compile_fail,
     ['T12788.hs', '-v0 ' + config.ghc_th_way_flags])
test('T12977', normal, compile, ['-v0'])
test('T12993', normal, multimod_compile, ['T12993.hs', '-v0'])
test('T13018', normal, compile, ['-v0'])
test('T13123', normal, compile, ['-v0'])
test('T13098', normal, compile, ['-v0'])
test('T11046', normal, multimod_compile, ['T11046','-v0'])
test('T13366C',
     [req_c, expect_broken_for(13366, ghci_ways)],
     compile_and_run,
     ['-v0'])
test('T13366Cxx',
     [req_c, expect_broken_for(13366, ghci_ways)],
     compile_and_run,
     ['-package system-cxx-std-lib -v0'])
test('T13473', normal, multimod_compile_and_run,
     ['T13473.hs', '-v0 ' + config.ghc_th_way_flags])
test('T13587',
     [omit_ghci # fails during compilation phase, remove after !10563
     , expect_broken(13587)],
     compile_and_run, ['-v0'])
test('T13618', normal, compile_and_run, ['-v0'])
test('T13642', normal, compile, ['-v0'])
test('T13781', normal, compile, ['-v0'])
test('T13782', normal, compile, [''])
test('T13837', normal, compile_fail, ['-v0 -dsuppress-uniques'])
test('T13856', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T13885', normal, compile_and_run, ['-v0'])
test('T13887', normal, compile_and_run, ['-v0'])
test('T13968', normal, compile_fail, ['-v0'])
test('T14204', normal, compile_fail, ['-v0'])
test('T14060', normal, compile_and_run, ['-v0'])
test('T14646', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T14681', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T14817', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T14843', normal, compile, ['-v0'])
test('T13776', normal, compile, ['-ddump-splices -v0'])
test('T14869', normal, compile,
    ['-v0 -ddump-splices -dsuppress-uniques ' + config.ghc_th_way_flags])
test('T14888', normal, compile,
    ['-v0 -ddump-splices -dsuppress-uniques ' + config.ghc_th_way_flags])
# There is a phasing issue in the ghci way which prevents us from being
# able to compile and link the foreign file while compiling the haskell module.
test('T14298', [req_c, expect_broken_for(15161, ghci_ways)], compile_and_run, ['-v0'])
test('T14875', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T14885a', normal, compile, [''])
test('T14885b', normal, compile, [''])
test('T14885c', normal, compile, [''])
test('T15243', normal, compile, ['-dsuppress-uniques'])
test('T15270A', normal, compile_fail, [''])
test('T15270B', normal, compile_fail, [''])
test('T15321', normal, compile_fail, [''])
test('T15324', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T15331', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T15365', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T15360a', normal, compile, [''])
test('T15360b', normal, compile_fail, [''])
test('T15362', normal, compile_fail,['-v0'])
# Note: T9693 should be only_ways(['ghci']) once it's fixed.
test('T9693', expect_broken(9693), ghci_script, ['T9693.script'])
test('T14471', normal, compile, [''])
test('TH_rebindableAdo', normal, compile, [''])
test('T14627', normal, compile_fail, [''])
test('TH_invalid_add_top_decl', normal, compile_fail, [''])
test('T15550', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T15502', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T15518', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T15572', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_implicitParams', normal, compile_and_run, ['-v0 -dsuppress-uniques'])
test('TH_implicitParamsErr1', normal, compile_fail, ['-v0 -dsuppress-uniques'])
test('TH_implicitParamsErr2', normal, compile_fail, ['-v0 -dsuppress-uniques'])
test('TH_implicitParamsErr3', normal, compile_fail, ['-v0 -dsuppress-uniques'])
test('TH_recursiveDo', normal, compile_and_run, ['-v0 -dsuppress-uniques'])
test('T15481', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_recover_warns', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T15738', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T15783', normal, multimod_compile,
    ['T15783A', '-v0 ' + config.ghc_th_way_flags])
test('T15792', normal, compile, ['-v0 -dsuppress-uniques'])
test('T15815', normal, multimod_compile,
    ['T15815B', '-v0 ' + config.ghc_th_way_flags])
test('T15843', [], multimod_compile_and_run,
     ['T15843.hs',
      '-v0 ' + config.ghc_th_way_flags])
test('T15845', normal, compile, ['-v0 -dsuppress-uniques'])
test('T15437', expect_broken(15437), multimod_compile,
     ['T15437', '-v0 ' + config.ghc_th_way_flags])
test('T15985', normal, compile, [''])
test('T16133', normal, compile_fail, [''])
test('T15471', normal, multimod_compile, ['T15471.hs', '-v0'])
test('T16180',
     [when(llvm_build(), expect_broken_for(16541, ['ext-interp'])),
      req_asm,
      # Ideally OpenBSD should have expect_broken_for(14012, ['ext-interp']).
      # Except the failure is in compilation so skip seems the best we can do.
      when(opsys('openbsd'), skip),
      expect_broken_for(16541, ghci_ways)],
     compile_and_run, ['-package ghc'])
test('T16183', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T16195', normal, multimod_compile, ['T16195.hs', '-v0'])
test('T16293b', normal, compile, [''])
test('T16326_TH', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T14741', normal, compile_and_run, [''])
test('T16666', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T16881', normal, compile_and_run, [''])
test('T16895a', normal, compile_fail, [''])
test('T16895b', normal, compile_fail, [''])
test('T16895c', normal, compile_fail, [''])
test('T16895d', normal, compile_fail, [''])
test('T16895e', normal, compile_fail, [''])
test('T16976', normal, compile, [''])
test('T16976f', normal, compile_fail, [''])
test('T16976z', normal, compile_fail, [''])
test('T16980', normal, compile, [''])
test('T16980a', normal, compile_fail, [''])
test('T17270a', extra_files(['T17270.hs']), multimod_compile, ['T17270', '-v0'])
test('T17270b', extra_files(['T17270.hs']), multimod_compile, ['T17270', '-fenable-th-splice-warnings -v0'])
test('T17296', normal, compile, ['-v0'])
test('T17305', normal, compile, ['-v0'])
test('T17380', normal, compile_fail, [''])
test('T17394', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T17461', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T17511', normal, compile, [''])
test('T17608', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T17688a', normal, compile, [''])
test('T17688b', normal, compile, [''])
test('T18097', normal, compile, [''])
test('TH_PprStar', normal, compile, ['-v0 -dsuppress-uniques'])
test('TH_StringLift', normal, compile, [''])
test('TH_BytesShowEqOrd', normal, compile_and_run, [''])
test('T18102', normal, compile_fail, [''])
test('T18102b', extra_files(['T18102b_aux.hs']), compile_and_run, [''])
test('T18121', normal, compile, [''])
test('T19709c', normal, compile_fail, [''])
test('T19709d', normal, compile_fail, [''])
test('T18123', normal, compile, [''])
test('T18388', normal, compile, [''])
test('T18612', normal, compile, [''])
test('T18740c', normal, compile_fail, [''])
test('T18740d', normal, compile_fail, [''])
test('T19363', normal, compile_and_run, [''])
test('T19373', normal, compile_and_run, [''])
test('T19377', normal, compile, [''])
test('T17804', normal, compile, [''])
test('T19470', only_ways(['ghci']), ghci_script, ['T19470.script'])
test('T19737', normal, compile, [''])
test('T19759', normal, compile, [''])
test('T20060', normal, compile, [''])
test('T20179', normal, compile_fail, [''])
test('T17820a', normal, compile_fail, [''])
test('T17820b', normal, compile_fail, [''])
test('T21463', normal, compile, [''])
test('T17820c', normal, compile_fail, [''])
test('T17820d', normal, compile_fail, [''])
test('T17820e', normal, compile_fail, [''])
test('T20454', normal, compile_and_run, [''])
test('T21547', [extra_files(['T21547A.hs']), normalise_version('ghc-internal', 'base')], multimod_compile_fail, ['T21547', '-v0'])
test('T20590', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T20773', only_ways(['ghci']), ghci_script, ['T20773.script'])
test('T20884', normal, compile_fail, [''])
test('T21038', normal, compile, [''])
test('T20842', normal, compile_and_run, [''])
test('T15433a', [extra_files(['T15433_aux.hs'])], multimod_compile_fail, ['T15433a', '-v0'])
test('T15433b', [extra_files(['T15433_aux.hs'])], multimod_compile, ['T15433b', '-v0'])
test('T20711', normal, compile_and_run, [''])
test('T20868', normal, compile_and_run, [''])
test('T21077', [extra_files(['T21077_Lib.hs'])], multimod_compile, ['T21077', '-v0'])
test('Lift_ByteArray', normal, compile_and_run, [''])
test('T21920', normal, compile_and_run, [''])
test('T21723', normal, compile_and_run, [''])
test('T21942', normal, compile_and_run, [''])
test('T22784', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T22818', normal, compile, ['-v0'])
test('T22819', normal, compile, ['-v0'])
test('TH_fun_par', normal, compile, [''])
test('T23036', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T23203', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_typed1', normal, compile_and_run, [''])
test('TH_typed2', normal, compile_and_run, [''])
test('TH_typed3', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_typed4', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('TH_typed5', normal, compile_and_run, [''])
test('T21050', normal, compile_fail, [''])
test('T22559a', normal, compile_fail, [''])
test('T22559b', normal, compile_fail, [''])
test('T22559c', normal, compile_fail, [''])
test('T23525', normal, compile, [''])
test('CodeQ_HKD', normal, compile, [''])
test('T23748', normal, compile, [''])
test('T23796', normal, compile, [''])
test('T23829_timely', normal, compile, [''])
test('T23829_tardy', normal, warn_and_run, [''])
test('T23829_hasty', normal, compile_fail, [''])
test('T23829_hasty_b', normal, compile_fail, [''])
test('T23927', normal, compile_and_run, [''])
test('T23954', normal, compile_and_run, [''])
test('T23309', [extra_files(['T23309A.hs']), req_c], multimod_compile, ['T23309', '-v0 T23309.c -optc-fPIC'])
test('T23378', [extra_files(['T23378A.hs']), js_skip], multimod_compile, ['T23378', '-v0'])
test('T23962', normal, compile_and_run, [''])
test('T23968', normal, compile_and_run, [''])
test('T23971', normal, compile_and_run, [''])
test('T23986', normal, compile_and_run, [''])
test('T24111', normal, compile_and_run, [''])
test('T23719', normal, compile_fail, [''])
test('T24190', normal, compile_and_run, [''])
test('T24308', normal, compile_and_run, [''])
test('T14032a', normal, compile, [''])
test('T14032e', normal, compile_fail, ['-dsuppress-uniques'])
test('ListTuplePunsTH', [only_ways(['ghci']), extra_files(['ListTuplePunsTH.hs', 'T15843a.hs'])], ghci_script, ['ListTuplePunsTH.script'])
test('T24559', normal, compile, [''])
test('T24571', normal, compile, [''])
test('T24299', normal, compile, [''])
test('T24557a', normal, compile_fail, [''])
test('T24557b', normal, compile_fail, [''])
test('T24557c', normal, compile_fail, [''])
test('T24557d', normal, compile_fail, [''])
test('T24557e', normal, compile, [''])
test('T24702a', normal, compile, [''])
test('T24702b', normal, compile, [''])
test('T24837', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
test('T24911', normal, compile, ['-v0 -ddump-splices -dsuppress-uniques'])
