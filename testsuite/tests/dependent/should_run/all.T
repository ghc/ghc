

# test('T11311', normal, compile_and_run, [''])
test('T11964', normal, multimod_compile, ['T11964', '-v0'])
closed_eval_dir = '../../../../docs/dependent-haskell/examples/closed-evaluation/'
open_eval_dir = '../../../../docs/dependent-haskell/examples/open-evaluation/'
test('NativeOpenIdentity',
     extra_files([open_eval_dir + f for f in ['OpenAlias.hs', 'OpenProducer.hs',
                 'OpenSource.hs', 'RunOpenSource.hs', 'RunOpenSource.stdout', 'check-source.sh']]),
     run_command, ['bash check-source.sh "{compiler}" "{libdir}"'])
test('NativeLocalOpen',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeLocalOpenProducer.hs'])],
     run_command, ['bash NativeLocalOpen.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
test('NativeOpenCharFamily',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['CharacterCase.hs', 'CharacterExport.hs']])],
     run_command, ['bash NativeOpenCharFamily.sh "{compiler}" "{libdir}"'])
test('NativeOpenInstalledUpper',
     [only_ways(['normal']), extra_files([open_eval_dir + 'UnicodeExport.hs',
                                        'NativeOpenInstalledInterfaces.sh'])],
     run_command,
     ['bash NativeOpenInstalledUpper.sh "{compiler}" "{libdir}" "{ghc_pkg}"'])
test('NativeOpenInstalledPair',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files([open_eval_dir + 'PreludePairExport.hs',
                   'NativeOpenInstalledInterfaces.sh', 'InstalledStagedPair.hs',
                   'WrongRigidPair.hs', 'WrongGroundPair.hs', 'WrongPairNamespace.hs',
                   'WrongPairRule.hs'])],
     run_command, ['bash NativeOpenInstalledPair.sh "{compiler}" "{libdir}" "{ghc_pkg}" '
                   + config.ghc_th_way_flags])
test('NativeOpenPairCoercion',
     [req_process, req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeOpenPairCoercionUsage.hs', 'NativeOpenPairCoercionWorker.hs']),
      extra_run_opts(f'"{config.libdir}"')],
     compile_and_run, ['-package ghc -package transformers'])
test('NativeOpenPairLinkable',
     [req_interp, req_rts_linker, only_ways(['normal'])],
     run_command, ['bash NativeOpenPairLinkable.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
test('NativeOpenCharGhci',
     extra_files([open_eval_dir + f for f in ['OpenAlias.hs', 'OpenProducer.hs', 'OpenSource.hs']]),
     ghci_script, ['NativeOpenCharGhci.script'])
test('NativeOpenRetry',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeOpenRetryLib.hs'])],
     multimod_compile_and_run,
     ['NativeOpenRetry', '-Wno-partial-type-signatures ' + config.ghc_th_way_flags])
test('NativeOpenRetryInterface',
     [req_interp, req_rts_linker, only_ways(['normal'])],
     run_command, ['bash NativeOpenRetryInterface.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
test('NativeClosedLength',
     [req_interp, req_rts_linker, extra_files([closed_eval_dir + f for f in
                                  ['check-source.sh', 'Client.hs', 'ClosedLengthProducer.hs']])],
     run_command, ['bash check-source.sh "{compiler}" "{libdir}"'])
test('NativeClosedChar',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedCharLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedChar', '-Werror=unused-imports ' + config.ghc_th_way_flags])
test('NativeClosedLists',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedListsLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedLists', '-Werror=unused-imports -Wno-partial-type-signatures ' + config.ghc_th_way_flags])
test('NativeClosedReturnedPair',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeClosedReturnedPairLib.hs', 'NativeClosedReturnedPairRejects.hs'])],
     run_command, ['bash NativeClosedReturnedPair.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
test('NativeClosedEnums',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeClosedEnumA.hs', 'NativeClosedEnumB.hs', 'NativeClosedEnumReexport.hs'])],
     multimod_compile_and_run,
     ['NativeClosedEnums', '-dcore-lint -dstg-lint -Werror=unused-imports ' + config.ghc_th_way_flags])
test('NativeClosedCompoundLists',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedCompoundListsLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedCompoundLists', '-dcore-lint -dstg-lint ' + config.ghc_th_way_flags])
test('NativeClosedResponseNames', [req_interp, req_rts_linker, only_ways(['normal'])],
     compile, [config.ghc_th_way_flags])
test('NativeClosedChoices',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedChoiceLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedChoices', '-dcore-lint -dstg-lint ' + config.ghc_th_way_flags])
test('NativeClosedNestedDependencies',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeClosedListsLib.hs', 'NativeOpenRetryLib.hs'])],
     run_command, ['bash NativeClosedNestedDependencies.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
test('NativeSourceTable',
     extra_files(['../../../../docs/dependent-haskell/examples/qualified-pi/ClosedTableLibrary.hs',
                  '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_and_run,
     ['NativeSourceTable', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeNumeralGhci',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     ghci_script, ['NativeNumeralGhci.script'])
test('NativeLetIndependentRule',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_and_run,
     ['NativeLetIndependentRule', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeListBoundary',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs',
                  '../../../../docs/dependent-haskell/examples/packed-vector/OrdinaryVecClient.hs']),
     multimod_compile_and_run,
     ['NativeListBoundary', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeClosedSum',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedSumLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedSum', '-Werror=unused-imports ' + config.ghc_th_way_flags])
test('NativeClosedSumDependencies',
     [req_interp, req_rts_linker, only_ways(['normal'])],
     run_command, ['bash NativeClosedSumDependencies.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
