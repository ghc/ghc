

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
                 'OpenSource.hs', 'RunOpenSource.hs', 'RunOpenSource.stdout', 'check-source.sh']]),
     run_command, ['bash check-source.sh "{compiler}" "{libdir}"'])
test('NativeLocalOpen',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeLocalOpenProducer.hs', 'NativeLocalOpenSibling.hs', 'NativeLocalOpenDerived.hs',
                   open_eval_dir + 'LocalInstance.hs', open_eval_dir + 'LocalAssociated.hs',
                   open_eval_dir + 'LocalDataFamily.hs', open_eval_dir + 'IndependentFamily.hs'])],
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
test('NativeOpenInstalledNot',
     [only_ways(['normal']),
      extra_files([open_eval_dir + f for f in ['OpenMapWorker.hs', 'InstalledNotSource.hs', 'InstalledNotProof.hs']]
                  + ['NativeOpenInstalledInterfaces.sh'])],
     run_command, ['bash NativeOpenInstalledNot.sh "{compiler}" "{libdir}" "{ghc_pkg}"'])
test('NativeOpenInstalledMap',
     [only_ways(['normal']),
      extra_files([open_eval_dir + 'PreludeMapSource.hs', open_eval_dir + 'PreludeMapProof.hs',
                   open_eval_dir + 'PreludeFilterSource.hs', open_eval_dir + 'PreludeFilterProof.hs',
                   open_eval_dir + 'PreludeMapLengthSize.hs', open_eval_dir + 'PreludeMapLengthSource.hs',
                   open_eval_dir + 'PreludeMapLengthProof.hs',
                   'NativeOpenInstalledMapProducer.hs'])],
     run_command, ['bash NativeOpenInstalledMap.sh "{compiler}" "{libdir}"'])
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
                   'NativeClosedVectorPair.hs', 'NativeClosedIndexedRoute.hs'])],
     multimod_compile_and_run,
     ['NativeClosedIndexedVec', '-dcore-lint -dstg-lint ' + config.ghc_th_way_flags])
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
                   'NativeTypeCaseHiddenAlias.hs', 'NativeTypeCaseHiddenErased.hs'])],
     run_command, ['bash NativeTypeCase.sh "{compiler}" "{libdir}"'])
test('NativeTypeCaseReify',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files([open_eval_dir + 'NativeCase.hs', open_eval_dir + 'NativeNestedCase.hs'])],
     multimod_compile, ['NativeTypeCaseReify', '-v0 -dcore-lint ' + config.ghc_th_way_flags])

test('NativeSimplifier',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativeSimplifierProvider.hs', 'NativeSimplifierClient.hs',
                   'NativeSimplifierLocalRec.hs', 'NativePackageRuleClient.hs', 'NativePackageSpecialise.hs',
                   'NativePackageRuleCase.hs', 'implicit-existential-source/WitnessSchema.hs',
                   '../../../../docs/dependent-haskell/examples/erased-existentials/PackageRules.hs',
                   '../../../../docs/dependent-haskell/examples/erased-existentials/LexicalPackageRules.hs',
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

test('NativeRetainedPackage',
     [req_host_target_ghc, only_ways(['normal']),
      extra_files(['NativePackageOperand.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/DependentPair.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/DependentProjection.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/RetainedNat.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedLength.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/PackedProjection.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/RetainedWitness.hs',
                   '../../../../docs/dependent-haskell/examples/packed-vector/RetainedProjection.hs'])],
     run_command, ['bash NativeRetainedPackage.sh "{compiler}" "{libdir}"'])

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
