

# test('T11311', normal, compile_and_run, [''])
test('T11964', normal, multimod_compile, ['T11964', '-v0'])
closed_eval_dir = '../../../../docs/dependent-haskell/examples/closed-evaluation/'
open_eval_dir = '../../../../docs/dependent-haskell/examples/open-evaluation/'
boundary_dir = '../../../../docs/dependent-haskell/examples/boundary-evidence/'
test('NativeMapWitnessInference',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files([boundary_dir + f for f in ['MapWitness.hs', 'MapRuleReplacement.hs',
                                              'UseMapWitness.hs', 'check-map-witness.sh']])],
     run_command, ['bash check-map-witness.sh "{compiler}" "{libdir}"'])
test('NativeOpenIdentity',
     extra_files([open_eval_dir + f for f in ['OpenAlias.hs', 'OpenProducer.hs',
                 'OpenSource.hs', 'RunOpenSource.hs', 'RunOpenSource.stdout', 'check-source.sh',
                 'RuntimeOrdering.hs', 'OrderingPayload.hs']]),
     run_command, ['bash check-source.sh "{compiler}" "{libdir}"'])
test('NativeLocalOpen',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeLocalOpenProducer.hs', 'NativeLocalOpenSibling.hs', 'NativeLocalOpenDerived.hs',
                   open_eval_dir + 'LocalInstance.hs', open_eval_dir + 'LocalAssociated.hs',
                   open_eval_dir + 'LocalDataFamily.hs', open_eval_dir + 'IndependentFamily.hs',
                   open_eval_dir + 'LocalAliasField.hs'])],
     run_command, ['bash NativeLocalOpen.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
test('NativeImportedFamily',
     [only_ways(['normal']),
      extra_files([open_eval_dir + 'ImportedFamilyProvider.hs', open_eval_dir + 'ImportedFamily.hs',
                   open_eval_dir + 'MixedFamily.hs', open_eval_dir + 'FamilySignature.hs'])],
     run_command, ['bash NativeImportedFamily.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
test('NativeOpenConstructor',
     [only_ways(['normal']), extra_files([open_eval_dir + 'OpenConstructorWorker.hs',
                                         open_eval_dir + 'OpenConstructorSource.hs',
                                         open_eval_dir + 'SpineWorker.hs'])],
     run_command, ['bash NativeOpenConstructor.sh "{compiler}" "{libdir}"'])
test('NativeOpenConstructorHelpers',
     [only_ways(['normal']), extra_files([open_eval_dir + 'SpineWorker.hs'])],
     run_command, ['bash NativeOpenConstructorHelpers.sh "{compiler}" "{libdir}"'])
test('NativeOpenBuild',
     [only_ways(['normal']), extra_files([open_eval_dir + 'OpenBuildWorker.hs',
                                         open_eval_dir + 'OpenBuildSource.hs', 'NativeOpenBuildControls.hs'])],
     run_command, ['bash NativeOpenBuild.sh "{compiler}" "{libdir}"'])
test('NativeOpenMap',
     [req_interp, only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['OpenMapWorker.hs', 'OpenMapSource.hs', 'OpenMapProof.hs', 'SpineWorker.hs']]
                  + ['../../../../docs/dependent-haskell/examples/qualified-pi/ClosedTableLibrary.hs'])],
     run_command, ['bash NativeOpenMap.sh "{compiler}" "{libdir}"'])
test('NativeOpenCharFamily',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['CharacterCase.hs', 'CharacterExport.hs']])],
     run_command, ['bash NativeOpenCharFamily.sh "{compiler}" "{libdir}"'])
test('NativeOpenInstalledUpper',
     [only_ways(['normal']), extra_files([open_eval_dir + 'UnicodeExport.hs',
                                        'NativeOpenInstalledInterfaces.sh'])],
     run_command,
     ['bash NativeOpenInstalledUpper.sh "{compiler}" "{libdir}" "{ghc_pkg}"'])
test('NativeBinaryEnum',
     [req_interp, req_rts_linker, only_ways(['normal'])],
     run_command, ['bash NativeBinaryEnum.sh "{compiler}" "{libdir}"'])
test('NativeOpenInstalledNot',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['OpenMapWorker.hs', 'InstalledNotSource.hs', 'InstalledNotProof.hs']]
                  + ['NativeOpenInstalledInterfaces.sh'])],
     run_command, ['bash NativeOpenInstalledNot.sh "{compiler}" "{libdir}" "{ghc_pkg}"'])
test('NativeOpenInstalledMap',
     [only_ways(['normal']),
      extra_files([open_eval_dir + 'PreludeMapSource.hs', open_eval_dir + 'PreludeMapProof.hs',
                   open_eval_dir + 'PreludeFilterSource.hs', open_eval_dir + 'PreludeFilterProof.hs',
                   open_eval_dir + 'PreludeFilterLengthProof.hs', open_eval_dir + 'PreludeFilterRuntime.hs',
                   open_eval_dir + 'PreludeFilterDecider.hs',
                   open_eval_dir + 'PreludeFilterAppendProof.hs', open_eval_dir + 'PreludeFilterAppendRuntime.hs',
                   open_eval_dir + 'PreludeAppendLengthSource.hs',
                   open_eval_dir + 'PreludeMapLengthSize.hs', open_eval_dir + 'PreludeMapLengthSource.hs',
                   open_eval_dir + 'PreludeMapLengthProof.hs',
                   'NativeOpenInstalledMapProducer.hs', 'NativeOpenFilterGivenControls.hs',
                   'NativeRetainedSourceOperand.hs'])],
     run_command, ['bash NativeOpenInstalledMap.sh "{compiler}" "{libdir}"'])
test('NativeOpenFilterCutoff',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs',
                   'PreludeFilterSource.hs', 'PreludeFilterLengthProof.hs', 'PreludeFilterRuntime.hs',
                   'PreludeFilterCutoff.hs']])],
     run_command, ['bash NativeOpenFilterCutoff.sh "{compiler}" "{libdir}"'])
test('NativeOpenInstalledTakeWhile',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs',
                   'PreludeFilterSource.hs', 'PreludeFilterLengthProof.hs', 'PreludeFilterRuntime.hs',
                   'PreludeTakeWhile.hs']])],
     run_command, ['bash NativeOpenInstalledTakeWhile.sh "{compiler}" "{libdir}"'])
test('NativeOpenInstalledDropWhile',
     [only_ways(['normal']),
      extra_files(['NativeOpenSpanShapes.hs'] +
                  [open_eval_dir + f for f in ['PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs',
                   'PreludeAppendLengthSource.hs', 'PreludeFilterSource.hs', 'PreludeFilterLengthProof.hs',
                   'PreludeFilterRuntime.hs', 'PreludeFilterAppendProof.hs', 'PreludeTakeWhile.hs',
                   'PreludeDropWhileSource.hs', 'PreludeDropWhileProof.hs',
                   'PreludeSpanSource.hs', 'PreludeSpanProof.hs']])],
     run_command, ['bash NativeOpenInstalledDropWhile.sh "{compiler}" "{libdir}"'])
test('NativeOpenSpanGhci',
     [req_bco, req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeOpenSpanShapes.hs'])],
     run_command, ['bash NativeOpenSpanGhci.sh "{compiler}" "{libdir}"'])
test('NativeOpenUserGuardedTail', [only_ways(['normal'])],
     run_command, ['bash NativeOpenUserGuardedTail.sh "{compiler}" "{libdir}"'])
test('NativeOpenConcreteGuardedList',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['ConcreteGuardedList.hs',
                   'ConcreteGuardedListSource.hs', 'ConcreteGuardedListProof.hs']])],
     run_command, ['bash NativeOpenConcreteGuardedList.sh "{compiler}" "{libdir}"'])
test('NativeOpenConcreteGuardedListGhci',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['ConcreteGuardedList.hs',
                   'ConcreteGuardedListSource.hs', 'ConcreteGuardedListProof.hs']])],
     run_command, ['bash NativeOpenConcreteGuardedListGhci.sh "{compiler}" "{libdir}"'])

test('NativeOpenInstalledAppend',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['PreludeAppendLengthSource.hs', 'PreludeAppendLengthProof.hs',
                                               'PreludeMapAppendProof.hs', 'PreludeMapCompositionSource.hs',
                                               'PreludeMapCompositionProof.hs',
                                               'PreludeMapLengthSize.hs', 'PreludeMapLengthSource.hs',
                                               'PreludeMapLengthProof.hs']])],
     run_command, ['bash NativeOpenInstalledAppend.sh "{compiler}" "{libdir}"'])
test('NativeOpenInstalledReverse',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['PreludeReverseLengthSource.hs', 'PreludeReverseLengthProof.hs',
                                               'PreludeAppendLengthSource.hs', 'PreludeMapLengthSize.hs',
                                               'PreludeMapLengthSource.hs', 'PreludeMapLengthProof.hs']]
                  + ['NativeOpenInstalledReverseProducer.hs'])],
     run_command, ['bash NativeOpenInstalledReverse.sh "{compiler}" "{libdir}"'])

test('NativeOpenIndexedAppend',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/' + f
                   for f in ['PackedVecLibrary.hs', 'OpenVectorAppend.hs', 'OrdinaryOpenVectorAppend.hs']])],
     run_command, ['bash NativeOpenIndexedAppend.sh "{compiler}" "{libdir}" ' + config.ghc_th_way_flags])
test('NativeOpenIndexedAppendGhci',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/' + f
                   for f in ['PackedVecLibrary.hs', 'OpenVectorAppend.hs', 'OrdinaryOpenVectorAppend.hs']])],
     run_command, ['bash NativeOpenIndexedAppendGhci.sh "{compiler}" "{libdir}" ' + config.ghc_th_way_flags])
test('NativeOpenListFoldr',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['ListFoldrSource.hs', 'ListFoldrProof.hs', 'PreludeFoldrProof.hs',
                                               'PreludeAppendLengthSource.hs', 'PreludeMapLengthSize.hs',
                                               'PreludeMapLengthSource.hs', 'PreludeMapLengthProof.hs']]
                  + ['NativeOpenListFoldrProducer.hs', 'NativeOpenListFoldrConcrete.hs'])],
     run_command, ['bash NativeOpenListFoldr.sh "{compiler}" "{libdir}"'])
test('NativeOpenListFoldrGhci',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['ListFoldrSource.hs', 'ListFoldrProof.hs', 'PreludeFoldrProof.hs',
                                               'PreludeAppendLengthSource.hs', 'PreludeMapLengthSize.hs',
                                               'PreludeMapLengthSource.hs', 'PreludeMapLengthProof.hs']]
                  + ['NativeOpenListFoldrProducer.hs', 'NativeOpenListFoldrConcrete.hs'])],
     run_command, ['bash NativeOpenListFoldrGhci.sh "{compiler}" "{libdir}"'])
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
test('NativeRetainedChar',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/qualified-pi/' + f
                   for f in ['RetainedCharacters.hs', 'RetainedCharacterClient.hs']])],
     run_command, ['bash NativeRetainedChar.sh "{compiler}" "{libdir}"'])
test('NativeRetainedNatural',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/qualified-pi/' + f
                   for f in ['RetainedNaturals.hs', 'RetainedNaturalClient.hs']])],
     run_command, ['bash NativeRetainedNatural.sh "{compiler}" "{libdir}"'])
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
test('NativeClosedLazyParcel',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files([closed_eval_dir + 'LazyParcel.hs'])],
     multimod_compile_and_run, ['NativeClosedLazyParcel', '-dcore-lint -dstg-lint ' + config.ghc_th_way_flags])
test('NativeClosedPartialPair',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedFamilyProjection.hs'])],
     run_command, ['bash NativeClosedPartialPair.sh "{compiler}" "{libdir}" ' + config.ghc_th_way_flags])
test('NativeClosedComputedPair',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files([closed_eval_dir + f for f in
                   ['ComputedPair.hs', 'ComputedPairProof.hs', 'PairProjection.hs']])],
     run_command, ['bash NativeClosedComputedPair.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
test('NativeClosedReturnedPair',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeClosedReturnedPairLib.hs', 'NativeClosedReturnedPairRejects.hs',
                   'NativeClosedFamilyProjection.hs', 'NativeClosedFamilyExpected.hs'])],
     run_command, ['bash NativeClosedReturnedPair.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
test('NativeClosedEnums',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeClosedEnumA.hs', 'NativeClosedEnumB.hs', 'NativeClosedEnumReexport.hs',
                   closed_eval_dir + 'FiniteTree.hs'])],
     multimod_compile_and_run,
     ['NativeClosedEnums', '-dcore-lint -dstg-lint -Werror=unused-imports ' + config.ghc_th_way_flags])
test('NativeClosedIndexed',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files([closed_eval_dir + 'IndexedMark.hs'])],
     multimod_compile_and_run,
     ['NativeClosedIndexed', '-dcore-lint -dstg-lint ' + config.ghc_th_way_flags])
test('NativeClosedIndexedVec',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files([closed_eval_dir + 'IndexedVec.hs', closed_eval_dir + 'ExistentialVec.hs',
                   'NativeClosedVectorPair.hs', 'NativeClosedIndexedRoute.hs',
                   closed_eval_dir + 'VectorSplitObservation.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs'])],
     multimod_compile_and_run,
     ['NativeClosedIndexedVec', '-dcore-lint -dstg-lint ' + config.ghc_th_way_flags])
test('NativeClosedWitnessedBatch',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files([closed_eval_dir + f for f in ['WitnessedBatch.hs', 'IndexedVec.hs', 'ExistentialVec.hs']])],
     run_command, ['bash NativeClosedWitnessedBatch.sh "{compiler}" "{libdir}" ' + config.ghc_th_way_flags])
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
test('NativeClosedFusedRule',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedFusedRuleLib.hs'])],
     run_command, ['bash NativeClosedFusedRule.sh "{compiler}" "{libdir}" ' + config.ghc_th_way_flags])
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
test('NativeInferredFamilyGhci', normal, ghci_script, ['NativeInferredFamilyGhci.script'])
test('NativeLetIndependentRule',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_and_run,
     ['NativeLetIndependentRule', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeListBoundary',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs',
                  '../../../../docs/dependent-haskell/examples/packed-vector/OrdinaryVecClient.hs']),
     multimod_compile_and_run,
     ['NativeListBoundary', '-fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeRetainedTypeWitness',
     [only_ways(['normal']),
      extra_files(['NativeRetainedTypeWitnessLib.hs', 'NativeRetainedTypeWitness.hs',
                   'NativeRetainedTypeOperand.hs'])],
     run_command, ['bash NativeRetainedTypeWitness.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])
test('NativeClosedSum',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedSumLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedSum', '-Werror=unused-imports ' + config.ghc_th_way_flags])
test('NativeClosedNestedExpected',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedSumLib.hs'])],
     multimod_compile_and_run,
     ['NativeClosedNestedExpected', '-dcore-lint -dstg-lint ' + config.ghc_th_way_flags])
test('NativeClosedSumDependencies',
     [req_interp, req_rts_linker, only_ways(['normal'])],
     run_command, ['bash NativeClosedSumDependencies.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])

test('NativeClosedCallback',
     [req_interp, req_rts_linker, only_ways(['normal']), extra_files(['NativeClosedCallbackLib.hs'])],
     run_command, ['bash NativeClosedCallback.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])

test('NativeUnsaturatedFamily',
     [only_ways(['normal']),
      extra_files(['NativeUnsaturatedProvider.hs', 'NativeUnsaturatedClient.hs',
                   'NativeUnsaturatedInverse.hs', 'NativeUnsaturatedEta.hs'])],
     run_command, ['bash NativeUnsaturatedFamily.sh "{compiler}" "{libdir}"'])
test('NativeMatchableProjection',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['MatchableEquality.hs', 'MatchableView.hs']]
                  + ['../../../../docs/dependent-haskell/examples/qualified-pi/RetainedCharacters.hs',
                     'NativeMatchableKindProjection.hs', 'NativeNestedMatchableProjection.hs'])],
     run_command, ['bash NativeMatchableProjection.sh "{compiler}" "{libdir}"'])
test('NativeLaterMatchability',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['AnchorReceiver.hs', 'LaterMatchability.hs',
                                              'LaterMatchabilityClient.hs']])],
     run_command, ['bash NativeLaterMatchability.sh "{compiler}" "{libdir}"'])
test('NativeIndexedClassReceiver',
     [only_ways(['normal']),
      extra_files([open_eval_dir + 'IndexedMap.hs', open_eval_dir + 'IndexedMapClient.hs',
                   'NativeIndexedClassReceiver.hs'])],
     run_command, ['bash NativeIndexedClassReceiver.sh "{compiler}" "{libdir}"'])
test('NativeUnsaturatedInvisible',
     [only_ways(['normal']),
      extra_files([open_eval_dir + 'InvisibleFamily.hs', 'NativeUnsaturatedInvisibleConsumer.hs',
                   'NativeUnsaturatedInvisibleInverse.hs', 'NativeUnsaturatedInvisibleMatchable.hs'])],
     run_command, ['bash NativeUnsaturatedInvisible.sh "{compiler}" "{libdir}"'])

test('NativeTypeLambda',
     [only_ways(['normal']),
      extra_files([open_eval_dir + 'TypeLambda.hs', 'NativeTypeLambdaScope.hs'])],
     run_command, ['bash NativeTypeLambda.sh "{compiler}" "{libdir}"'])
test('NativeTypeLambdaReify',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files([open_eval_dir + 'TypeLambda.hs', 'NativeTypeLambdaReifyClient.hs'])],
     multimod_compile, ['NativeTypeLambdaReifyClient', '-v0 -dcore-lint ' + config.ghc_th_way_flags])

test('NativeCSEProjection',
     [only_ways(['normal']), extra_files(['NativeCSEProjectionLib.hs'])],
     multimod_compile_and_run, ['NativeCSEProjection', '-O1 -dcore-lint -dstg-lint'])

test('NativeScopedSumMatch',
     [only_ways(['normal']),
      extra_files(['NativeScopedSumMatchLeft.hs', 'NativeScopedSumMatchRight.hs',
                   'NativeScopedSumMatchClient.hs', 'NativeScopedSumMatchReject.hs'])],
     run_command, ['bash NativeScopedSumMatch.sh "{compiler}" "{libdir}"'])

test('NativeScopedSumSignatureReify',
     [req_host_target_ghc, req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeScopedSumSignatureReifyProvider.hs',
                   'NativeScopedSumSignatureReifyClient.hs'])],
     run_command, ['bash NativeScopedSumSignatureReify.sh "{compiler}" "{libdir}" '
                   + config.ghc_th_way_flags])

test('NativeGadtRetainedSum',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeGadtRetainedSumProvider.hs', 'NativeGadtRetainedSumClient.hs'])],
     run_command, ['bash NativeGadtRetainedSum.sh "{compiler}" "{libdir}"'])

test('NativeAlternatingBinders',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeAlternatingBindersProvider.hs', 'NativeAlternatingBindersClient.hs'])],
     run_command, ['bash NativeAlternatingBinders.sh "{compiler}" "{libdir}"'])

test('NativeRetainedMatchability',
     [req_host_target_ghc, only_ways(['normal'])],
     run_command, ['bash NativeRetainedMatchability.sh "{compiler}" "{libdir}"'])

test('NativeNestedPackagePat',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeGadtRetainedSumProvider.hs', 'NativeNestedPackagePatProvider.hs',
                   'NativeNestedPackagePatClient.hs', 'NativeNestedPackagePatBad.hs'])],
     run_command, ['bash NativeNestedPackagePat.sh "{compiler}" "{libdir}"'])

test('NativeRetainedIndexHpc',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeRetainedIndexHpcProvider.hs', 'NativeRetainedIndexHpcClient.hs'])],
     run_command, ['bash NativeRetainedIndexHpc.sh "{compiler}" "{libdir}" "{hpc}"'])

test('NativeTypeLet',
     [only_ways(['normal']), extra_files([open_eval_dir + 'TypeLet.hs'])],
     run_command, ['bash NativeTypeLet.sh "{compiler}" "{libdir}"'])

test('NativeClosedMatchability',
     [only_ways(['normal']),
      extra_files(['NativeClosedMatchabilityProvider.hs',
                   'NativeClosedMatchabilityClient.hs', 'NativeClosedMatchabilityConflict.hs'])],
     run_command, ['bash NativeClosedMatchability.sh "{compiler}" "{libdir}"'])

test('NativeOpenMatchability',
     [only_ways(['normal']),
      extra_files(['../should_compile/T14038.hs', '../../polykinds/T14174a.hs',
                   '../../polykinds/T15874.hs',
                   open_eval_dir + 'NestedFunctionKind.hs', open_eval_dir + 'NestedFunctionClient.hs',
                   'NativeOpenMatchabilityClient.hs', 'NativeOpenMatchabilityConflict.hs'])],
     run_command, ['bash NativeOpenMatchability.sh "{compiler}" "{libdir}"'])

test('NativePercentArrow',
     [only_ways(['normal']),
      extra_files(['NativePercentArrowProvider.hs', 'NativePercentArrowOnlyModifiers.hs',
                   'NativePercentArrowDuplicate.hs', 'NativePercentArrowWrong.hs',
                   'NativePercentArrowNoModifiers.hs'])],
     run_command, ['bash NativePercentArrow.sh "{compiler}" "{libdir}"'])
test('NativePercentArrowQuote',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativePercentArrowProvider.hs'])],
     multimod_compile, ['NativePercentArrowQuote', '-v0 -dcore-lint ' + config.ghc_th_way_flags])

test('NativePercentArrowPpr',
     [req_interp, only_ways(['normal']), extra_files(['../should_fail/BadTelescope.hs'])],
     run_command, ['bash NativePercentArrowPpr.sh "{compiler}" "{libdir}"'])

test('NativeTypeCase',
     [only_ways(['normal']),
      extra_files([open_eval_dir + 'NativeCase.hs', open_eval_dir + 'NativeNestedCase.hs',
                   open_eval_dir + 'OpenMapWorker.hs',
                   'NativeTypeCaseClient.hs', 'NativeTypeCaseRigid.hs',
                   'NativeTypeCaseInvalid.hs', 'NativeTypeCaseEscape.hs', 'NativeTypeCaseBadPattern.hs',
                   'NativeTypeCaseFallback.hs', 'NativeTypeCaseFallbackRigid.hs', 'NativeTypeCaseDisabled.hs', 'NativeTypeCaseBranchKind.hs',
                   'NativeTypeCaseForallResult.hs', 'NativeTypeCaseSignatureDistinct.hs',
                   'NativeTypeCaseScrutinee.hs', 'NativeTypeCaseLetMonomorphic.hs',
                   'NativeTypeCaseLetWrong.hs', 'NativeTypeCaseLetUnowned.hs',
                   'NativeTypeCaseHeadProvider.hs', 'NativeTypeCaseHeads.hs',
                   'NativeTypeCaseFamilyHead.hs', 'NativeTypeCaseSynonymCatchAll.hs',
                   'NativeTypeCaseDominatedRhs.hs', 'NativeTypeCaseDominatedWrongKind.hs',
                   'NativeTypeCaseNestedAliasHead.hs', 'NativeTypeCaseOpaqueIndex.hs',
                   'NativeTypeCaseErasedAlias.hs', 'NativeTypeCaseErasedWildcard.hs',
                   'NativeRetainedJoin.hs', 'NativeRetainedJoinProvider.hs',
                   'NativeTypeCaseHiddenAlias.hs', 'NativeTypeCaseHiddenErased.hs'])],
     run_command, ['bash NativeTypeCase.sh "{compiler}" "{libdir}"'])
test('NativeTypeCaseReify',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files([open_eval_dir + 'NativeCase.hs', open_eval_dir + 'NativeNestedCase.hs',
                   open_eval_dir + 'OpenMapWorker.hs'])],
     multimod_compile, ['NativeTypeCaseReify', '-v0 -dcore-lint ' + config.ghc_th_way_flags])
test('NativeTypeCaseRetainedTh',
     [req_interp, req_rts_linker, req_host_target_ghc, only_ways(['normal']),
      extra_files([open_eval_dir + 'NativeRetainedTH.hs',
                   'NativeTypeCaseRetainedThWire.hs', 'NativeTypeCaseRetainedThRaw.hs'])],
     run_command, ['bash NativeTypeCaseRetainedTh.sh "{compiler}" "{libdir}"'])
test('NativeTypeCaseRetainedConTh',
     [req_interp, req_rts_linker, req_host_target_ghc, only_ways(['normal']),
      extra_files(['../should_fail/NativeTypeCaseRetainedReify.hs'])],
     run_command, ['bash NativeTypeCaseRetainedConTh.sh "{compiler}" "{libdir}"'])

test('NativeRetainedFunctionRules',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/qualified-pi/' + f for f in
                   ['RetainedRules.hs', 'RetainedRulesClient.hs', 'check-retained-rules.sh',
                    'VisibleRuleArguments.hs', 'VisibleRuleClient.hs', 'check-visible-rule-arguments.sh',
                    'check-imported-retained-rules.sh']])],
     run_command, ['bash check-retained-rules.sh "{compiler}" "{libdir}"'])
test('NativeSimplifier',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeSimplifierProvider.hs', 'NativeSimplifierClient.hs',
                   'NativeSimplifierLocalRec.hs', 'NativePackageRuleClient.hs', 'NativePackageSpecialise.hs',
                   'NativePackageRuleCase.hs', 'implicit-existential-source/WitnessSchema.hs',
                   '../../../../docs/dependent-haskell/examples/erased-existentials/PackageRules.hs',
                   '../../../../docs/dependent-haskell/examples/erased-existentials/LexicalPackageRules.hs',
                   '../../../../docs/dependent-haskell/examples/erased-existentials/OrphanPackageRules.hs',
                   '../../../../docs/dependent-haskell/examples/erased-existentials/ErasedStream.hs',
                   '../../../../docs/dependent-haskell/examples/erased-existentials/StreamFold.hs'])],
     run_command, ['bash NativeSimplifier.sh "{compiler}" "{libdir}"'])

test('NativeOptimizedReadback',
     [req_host_target_ghc, req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeClosedFamilyProjection.hs'])],
     run_command, ['bash NativeOptimizedReadback.sh "{compiler}" "{libdir}" ' + config.ghc_th_way_flags])

test('NativeFunctionIndex',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeFunctionIndexClient.hs',
                   '../../../../docs/dependent-haskell/examples/qualified-pi/FunctionFieldLibrary.hs',
                   '../../../../docs/dependent-haskell/examples/qualified-pi/FunctionFieldClient.hs',
                   '../../../../docs/dependent-haskell/examples/qualified-pi/ConstructorCallClient.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs'])],
     run_command, ['bash NativeFunctionIndex.sh "{compiler}" "{libdir}"'])

test('NativeIndexedGadtFields',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs',
                   'NativeIndexedGadtFieldsProvider.hs', 'NativeIndexedGadtFields.hs'])],
     run_command, ['bash NativeIndexedGadtFields.sh "{compiler}" "{libdir}"'])

test('NativeRetainedPackage',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativePackageOperand.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/ListVectorClient.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/StoredVector.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/ConservedPartition.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/DependentPair.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/DependentProjection.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/RetainedNat.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedLength.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedProjection.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/RetainedWitness.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/RetainedProjection.hs'])],
     run_command, ['bash NativeRetainedPackage.sh "{compiler}" "{libdir}"'])

test('NativeCertifiedSplit',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/' + f
                   for f in ['PackedVecLibrary.hs', 'CertifiedSplit.hs', 'CertifiedSplitClient.hs']])],
     run_command, ['bash NativeCertifiedSplit.sh "{compiler}" "{libdir}"'])

test('NativeLinearRetainedPackage',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeLinearRetainedPackage' + suffix for suffix in
                   ['Provider.hs', 'Client.hs', 'Reject.hs', 'EvidenceReject.hs', 'FallthroughReject.hs']])],
     run_command, ['bash NativeLinearRetainedPackage.sh "{compiler}" "{libdir}"'])
test('NativeNamedSplit',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/NamedSplitClient.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs'])],
     run_command, ['bash NativeNamedSplit.sh "{compiler}" "{libdir}"'])

test('NativeIndexedFin',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeIndexedFinGate.hs', 'NativeLinearIndexColumns.hs', 'NativeGeneralGadtOperandProvider.hs', 'NativeIndexedOperandWire.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']
                  + ['../../../../docs/dependent-haskell/examples/qualified-pi/' + f
                     for f in ['ParameterizedIndexLibrary.hs', 'FinitePosition.hs', 'FinitePositionClient.hs',
                               'ListCursor.hs', 'AlignedList.hs', 'CursorClient.hs']])],
     run_command, ['bash NativeIndexedFin.sh "{compiler}" "{libdir}"'])
test('NativeOrdinaryInline',
     [req_bco, req_host_target_ghc, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs',
                   '../../../../docs/dependent-haskell/examples/qualified-pi/ParameterizedIndexLibrary.hs',
                   '../../../../docs/dependent-haskell/examples/qualified-pi/FinitePosition.hs'])],
     run_command, ['bash NativeOrdinaryInline.sh "{compiler}" "{libdir}"'])
test('NativeOrdinaryInlinable',
     [req_host_target_ghc, only_ways(['normal'])],
     run_command, ['bash NativeOrdinaryInlinable.sh "{compiler}" "{libdir}"'])
test('NativeIndexedConsProof',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeIndexedConsProof.hs', 'NativeIndexedConsProofClient.hs',
                   'NativeIndexedOperandWire.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs'])],
     run_command, ['bash NativeIndexedConsProof.sh "{compiler}" "{libdir}"'])

test('NativeRetainedCast',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeRetainedCastClient.hs', 'NativeRetainedCastLength.hs',
                   'NativeRetainedCastNominal.hs', 'NativeLetIndependentRule.hs',
                   'NativeRetainedCastDomainWorker.hs', 'NativeRetainedCastDomainClient.hs',
                   'NativeRetainedCastDomainReject.hs',
                   '../../../../docs/dependent-haskell/examples/qualified-pi/ParameterizedIndexLibrary.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs'])],
     run_command, ['bash NativeRetainedCast.sh "{compiler}" "{libdir}"'])

test('NativePackageLength',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedLength.hs'])],
     run_command, ['bash NativePackageLength.sh "{compiler}" "{libdir}"'])

test('NativeDefaultVector',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/DefaultVector.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs'])],
     run_command, ['bash NativeDefaultVector.sh "{compiler}" "{libdir}"'])

test('NativeDefaultO1Interface',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeDefaultO1InterfaceProducer.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/DependentPair.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/DependentProjection.hs'])],
     run_command, ['bash NativeDefaultO1Interface.sh "{compiler}" "{libdir}"'])

test('NativeDepFunCoWire',
     [req_process, req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeDepFunCoClient.hs', 'NativeDepFunCoMutate.hs',
                   'NativeDepFunCoProvider.hs'])],
     run_command, ['bash NativeDepFunCoWire.sh "{compiler}" "{libdir}"'])

test('NativeMixedPackageInterface',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/erased-existentials/ForgetWitness.hs'])],
     run_command, ['bash NativeMixedPackageInterface.sh "{compiler}" "{libdir}"'])

test('NativeExistentialSource',
     [req_host_target_ghc, req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['erased-existential-source'])],
     run_command, ['bash NativeExistentialSource.sh "{compiler}" "{libdir}" ' + config.ghc_th_way_flags])

test('NativeImplicitExistential',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['implicit-existential-source'])],
     run_command, ['bash NativeImplicitExistential.sh "{compiler}" "{libdir}"'])

test('NativeExistentialBeta',
     [req_host_target_ghc, unless(have_ncg(), skip), only_ways(['normal'])],
     compile_and_run, ['-O1 -dcore-lint -dstg-lint'])

test('NativeConstrainedFamilyRules',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeConstrainedFamilyRulesProvider.hs', 'NativeConstrainedFamilyRulesClient.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']
                  + ['../../../../docs/dependent-haskell/examples/qualified-pi/' + f
                     for f in ['ParameterizedIndexLibrary.hs', 'FinitePosition.hs']])],
     run_command, ['bash NativeConstrainedFamilyRules.sh "{compiler}" "{libdir}"'])

test('NativePromotedClassBoundary',
     [req_host_target_ghc, only_ways(['normal']), extra_files(['promoted-class'])],
     run_command, ['bash NativePromotedClassBoundary.sh "{compiler}" "{libdir}"'])

test('NativeRetainedFamilyDomain',
     [only_ways(['normal']),
      extra_files(['../../../../docs/dependent-haskell/examples/open-evaluation/' + f
                   for f in ['ConstrainedElements.hs', 'ConstrainedElementsClient.hs']])],
     run_command, ['bash NativeRetainedFamilyDomain.sh "{compiler}" "{libdir}"'])

test('NativeConstrainedFamily',
     [only_ways(['normal']),
      extra_files(['NativeConstrainedFamilyProvider.hs', 'NativeConstrainedFamilyClient.hs',
                   'NativePartialFamilyScope.hs', 'NativeImprovedFamilyScope.hs',
                   'NativeFamilyEquationScope.hs'])],
     run_command, ['bash NativeConstrainedFamily.sh "{compiler}" "{libdir}"'])
