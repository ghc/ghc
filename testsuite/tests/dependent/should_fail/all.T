test('DepFail1', normal, compile_fail, [''])
test('RAE_T32a', req_th, compile_fail, [''])
test('TypeSkolEscape', normal, compile_fail, ['-fprint-explicit-runtime-reps'])
test('BadTelescope', normal, compile_fail, [''])
test('BadTelescope2', normal, compile_fail, [''])
test('BadTelescope3', normal, compile_fail, [''])
test('BadTelescope4', normal, compile_fail, [''])
test('BadTelescope5', normal, compile_fail, [''])
test('PromotedClass', normal, compile_fail, [''])
test('SelfDep', normal, compile_fail, [''])
test('SelfDepCls', normal, compile_fail, [''])
test('RenamingStar', normal, compile_fail, [''])
test('T11407', normal, compile_fail, [''])
test('T11334b', normal, compile_fail, [''])
test('T11471', normal, compile_fail, [''])
test('T12081', normal, compile_fail, [''])
test('T13135', normal, compile_fail, [''])
test('T13601', normal, compile_fail, [''])
test('T13780a', normal, compile_fail, [''])
test('T13780c', [extra_files(['T13780b.hs'])],
                multimod_compile_fail, ['T13780c', ''])
test('T13895', normal, compile_fail, [''])
test('T14066', normal, compile_fail, [''])
test('T14066c', normal, compile_fail, [''])
test('T14066d', normal, compile_fail, [''])
test('T14066e', normal, compile_fail, [''])
test('T14066f', normal, compile_fail, [''])
test('T14066g', normal, compile_fail, [''])
test('T14845_fail1', normal, compile_fail, [''])
test('T14845_fail2', normal, compile_fail, [''])
test('T14845_compile', normal, compile_fail, [''])
test('T15245', normal, compile_fail, [''])
test('T15215', normal, compile_fail, [''])
test('T15308', normal, compile_fail, ['-fno-print-explicit-kinds'])
test('T15343', normal, compile_fail, [''])
test('T15380', normal, compile_fail, [''])
test('T15588', normal, compile_fail, [''])
test('T15591b', normal, compile_fail, [''])
test('T15591c', normal, compile_fail, [''])
test('T15743c', normal, compile_fail, [''])
test('T15743d', normal, compile_fail, [''])
test('T15825', normal, compile_fail, [''])
test('T15859', normal, compile_fail, [''])
test('T15859a', normal, compile_fail, [''])
test('T15264', normal, compile_fail, [''])
test('T16326_Fail1', normal, compile_fail, [''])
test('T16326_Fail2', normal, compile_fail, [''])
test('T16326_Fail3', normal, compile_fail, [''])
test('T16326_Fail4', normal, compile_fail, [''])
test('T16326_Fail5', normal, compile_fail, [''])
test('T16326_Fail6', normal, compile_fail, [''])
test('T16326_Fail7', normal, compile_fail, [''])
test('T16326_Fail8', normal, compile_fail, [''])
test('T16326_Fail9', normal, compile_fail, [''])
test('T16326_Fail10', normal, compile_fail, [''])
test('T16326_Fail11', normal, compile_fail, [''])
test('T16326_Fail12', normal, compile_fail, [''])

test('T16391b', normal, compile_fail, ['-fprint-explicit-runtime-reps'])

# T16391b got an ASSERT error during -ddump-tc-trace (#19495)
# So we run it again with -ddump-tc-trace and a new name
# Little changes in the trace output don't matter; just search for "panic!"
test('T19495', [ grep_errmsg(r'panic!') ], compile_fail, ['-dsuppress-uniques -ddump-tc-trace'])

test('T16344', normal, compile_fail, [''])
test('T16344a', normal, compile_fail, [''])
test('T16418', normal, compile_fail, [''])
test('T17541', normal, compile_fail, [''])
test('T17541b', normal, compile_fail, [''])
test('T13135_simple', normal, compile_fail, [''])
test('T17131', normal, compile_fail, [''])
test('T14880', normal, compile_fail, [''])
test('T14880-2', normal, compile_fail, [''])
test('T15076', normal, compile_fail, [''])
test('T15076b', normal, compile_fail, [''])
test('T17687', normal, compile_fail, [''])
test('T18271', normal, compile_fail, [''])
test('NativeSourceTableWrong',
     extra_files(['../../../../docs/dependent-haskell/examples/qualified-pi/ClosedTableLibrary.hs']),
     multimod_compile_fail,
     ['NativeSourceTableWrong', '-v0 -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeSourceTablePartial', normal, compile_fail, [''])
test('NativeListBoundaryPartial',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_fail,
     ['NativeListBoundaryPartial', '-v0 -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeSourceFieldCoverage', normal, compile_fail, [''])
test('NativeSourceLengthRecursion', normal, compile_fail, [''])
test('NativeSourcePlusRecursion', normal, compile_fail, [''])
test('NativeSourceProductRecursion',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_fail,
     ['NativeSourceProductRecursion', '-v0 -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeSourceProductRule',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_fail,
     ['NativeSourceProductRule', '-v0 -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeLetMethodRule',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_fail,
     ['NativeLetMethodRule', '-v0 -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeSourceLengthFamily', req_th, compile_fail, [''])
test('NativeOpenTermLimit', normal, compile_fail, [''])
test('NativeOpenRetryWrong', [req_interp, req_rts_linker, only_ways(['normal'])],
     compile_fail, ['-fdefer-type-errors -Wno-partial-type-signatures ' + config.ghc_th_way_flags])
test('NativeOpenRetryUnknown', normal, compile_fail,
     ['-fdefer-type-errors -Wno-partial-type-signatures'])
test('NativeOpenRetryGiven', normal, compile_fail, ['-Wno-partial-type-signatures'])
test('NativeOpenRetryNoGen', normal, compile_fail, ['-Wno-partial-type-signatures'])
test('NativeOpenRetrySafe', normal, compile_fail, ['-Wno-partial-type-signatures'])
test('NativeOpenRetryQuote', req_th, compile_fail, ['-Wno-partial-type-signatures'])
test('NativeClosedEvalSafe',
     extra_files(['../../../../docs/dependent-haskell/examples/closed-evaluation/ClosedLengthProducer.hs']),
     multimod_compile_fail, ['NativeClosedEvalSafe', '-v0'])
test('NativeClosedCharRigid', normal, compile_fail, [''])
test('NativeClosedCharResult', normal, compile_fail, [''])
test('NativeClosedCharWildcard', normal, compile_fail, [''])
test('NativeClosedCharWrong', [req_interp, req_rts_linker, only_ways(['normal'])],
     compile_fail, [config.ghc_th_way_flags])
test('NativeClosedListsWrong',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedListsLib.hs'])],
     multimod_compile_fail, ['NativeClosedListsWrong', '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedListsBottom',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedListsLib.hs'])],
     multimod_compile_fail, ['NativeClosedListsBottom', '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedSourceMixed', normal, compile_fail, [''])
for name in ['NativeClosedSourceAmbiguous', 'NativeClosedLengthNoCast', 'NativeClosedResultDictionary']:
    test(name, [req_interp, req_rts_linker, only_ways(['normal'])],
         compile_fail, [config.ghc_th_way_flags])
test('NativeClosedComputedEmpty',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedListsLib.hs'])],
     multimod_compile_fail, ['NativeClosedComputedEmpty', '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedNestedKind',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedListsLib.hs'])],
     multimod_compile_fail, ['NativeClosedNestedKind', '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedNestedResultAmbiguous',
     [req_interp, req_rts_linker, only_ways(['normal'])],
     compile_fail, [config.ghc_th_way_flags])
for name in ['NativeClosedNestedResultDictionary', 'NativeClosedNestedResultNoCast']:
    test(name, [req_interp, req_rts_linker, only_ways(['normal']),
                extra_files(['../should_run/NativeClosedSumLib.hs'])],
         multimod_compile_fail, [name, '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedNestedMissing',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedListsLib.hs', '../should_run/NativeClosedSumLib.hs'])],
     multimod_compile_fail, ['NativeClosedNestedMissing', '-v0 ' + config.ghc_th_way_flags])
for name in ['NativeClosedCompoundListBottom', 'NativeClosedCompoundListUnsupported',
             'NativeClosedConstructorAmbiguous', 'NativeClosedConstructorEvidence']:
    test(name, [req_interp, req_rts_linker, only_ways(['normal']),
                extra_files(['../should_run/NativeClosedCompoundListsLib.hs'])],
         multimod_compile_fail, [name, '-v0 ' + config.ghc_th_way_flags])
for name in ['NativeClosedEnumAbstract', 'NativeClosedEnumPartial', 'NativeClosedEnumField',
             'NativeClosedEnumOperand', 'NativeClosedExpanding']:
    test(name, [req_interp, req_rts_linker, only_ways(['normal']),
                extra_files(['../should_run/NativeClosedEnumA.hs'])],
         multimod_compile_fail, [name, '-O1 -v0 ' + config.ghc_th_way_flags])
test('NativeClosedEnumKind',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedEnumA.hs', '../should_run/NativeClosedEnumB.hs'])],
     multimod_compile_fail, ['NativeClosedEnumKind', '-v0 ' + config.ghc_th_way_flags])
for name in ['NativeClosedLazyParcelDemand', 'NativeClosedLazyParcelEscape', 'NativeClosedLazyParcelKind']:
    test(name, [req_interp, req_rts_linker, only_ways(['normal']),
                extra_files(['../../../../docs/dependent-haskell/examples/closed-evaluation/LazyParcel.hs'])],
         multimod_compile_fail, [name, '-v0 ' + config.ghc_th_way_flags])
for name in ['NativeClosedPresentBottom', 'NativeClosedUnsupportedAlternative', 'NativeClosedOtherEither']:
    test(name, [req_interp, req_rts_linker, only_ways(['normal']),
                extra_files(['../should_run/NativeClosedChoiceLib.hs'])],
         multimod_compile_fail, [name, '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedIndexedContext',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['NativeClosedIndexedContextLib.hs'])],
     multimod_compile_fail, ['NativeClosedIndexedContext', '-v0 ' + config.ghc_th_way_flags])
for name in ['NativeClosedIndexedVecEmpty', 'NativeClosedIndexHidden', 'NativeClosedIndexFamily',
             'NativeClosedSomeVecAmbiguous']:
    test(name, [req_interp, req_rts_linker, only_ways(['normal']),
                extra_files(['NativeClosedIndexFailures.hs',
                             '../../../../docs/dependent-haskell/examples/closed-evaluation/IndexedVec.hs'])],
         multimod_compile_fail, [name, '-v0 ' + config.ghc_th_way_flags])
for name in ['NativeClosedSomeVecIndexVisibility', 'NativeClosedWitnessMismatch',
             'NativeClosedWitnessMissing']:
    test(name, [req_interp, req_rts_linker, only_ways(['normal']),
                extra_files(['NativeClosedWitnessFailures.hs'] +
                            ['../../../../docs/dependent-haskell/examples/closed-evaluation/' + f
                             for f in ['IndexedVec.hs', 'ExistentialVec.hs']])],
         multimod_compile_fail, [name, '-v0 ' + config.ghc_th_way_flags])
for name in ['NativeClosedIndexedWrongKind', 'NativeClosedIndexedWrongValue', 'NativeClosedIndexedPartial']:
    test(name, [req_interp, req_rts_linker, only_ways(['normal']),
                extra_files(['../../../../docs/dependent-haskell/examples/closed-evaluation/IndexedMark.hs'])],
         multimod_compile_fail, [name, '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedWrongAlternative', [req_interp, req_rts_linker, only_ways(['normal'])],
     compile_fail, [config.ghc_th_way_flags])
test('NativeClosedSaturatedKinds',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedListsLib.hs'])],
     multimod_compile_fail, ['NativeClosedSaturatedKinds', '-v0 ' + config.ghc_th_way_flags])
for test_name in ['NativeClosedSaturatedUnder', 'NativeClosedSaturatedOver']:
    test(test_name, [req_interp, req_rts_linker, only_ways(['normal'])],
         compile_fail, [config.ghc_th_way_flags])
test('NativeClosedSaturatedMissing',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedListsLib.hs', '../should_run/NativeClosedSumLib.hs'])],
     multimod_compile_fail, ['NativeClosedSaturatedMissing', '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedSaturatedAmbiguous',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedSumLib.hs'])],
     multimod_compile_fail, ['NativeClosedSaturatedAmbiguous', '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedNestedStrict',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedListsLib.hs'])],
     multimod_compile_fail, ['NativeClosedNestedStrict', '-v0 ' + config.ghc_th_way_flags])
test('NativeSplitResult',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_fail, ['NativeSplitResult', '-v0 -fomit-interface-pragmas'])
test('NativeSourceLengthEmpty',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_fail,
     ['NativeSourceLengthEmpty', '-v0 -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeTypeNumeralWrong',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_fail,
     ['NativeTypeNumeralWrong', '-v0 -fomit-interface-pragmas -fno-write-if-simplified-core'])
test('NativeMethodGiven',
     extra_files(['../../../../docs/dependent-haskell/examples/packed-vector/PackedVecLibrary.hs']),
     multimod_compile_fail,
     ['NativeMethodGiven', '-v0 -fdefer-type-errors -Wno-simplifiable-class-constraints -fomit-interface-pragmas'])
test('NativeClosedSumMissing',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedSumLib.hs'])],
     multimod_compile_fail,
     ['NativeClosedSumMissing', '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedSumLocal',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedSumLib.hs'])],
     multimod_compile_fail,
     ['NativeClosedSumLocal', '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedSumHidden',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedSumLib.hs'])],
     multimod_compile_fail,
     ['NativeClosedSumHidden', '-v0 ' + config.ghc_th_way_flags])
test('NativeClosedSumAmbiguous',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedSumLib.hs'])],
     multimod_compile_fail,
     ['NativeClosedSumAmbiguous', '-v0 ' + config.ghc_th_way_flags])

for name in ['NativeClosedComputedSiblingsAmbiguous', 'NativeClosedComputedSiblingsMismatch']:
    test(name, [req_interp, req_rts_linker, only_ways(['normal'])],
         compile_fail, [config.ghc_th_way_flags])

test('NativeClosedCallbackKinds',
     [req_interp, req_rts_linker, only_ways(['normal']),
      extra_files(['../should_run/NativeClosedCallbackLib.hs'])],
     multimod_compile_fail, ['NativeClosedCallbackKinds', '-v0 ' + config.ghc_th_way_flags])

test('NativeTypeLetQuote', [req_th, only_ways(['normal'])], compile_fail, [''])

test('NativePercentArrowQuoteUnknown', [req_th, only_ways(['normal'])],
     compile_fail, ['-fdiagnostics-show-caret'])

test('NativeTypeCaseQuote', [req_th, only_ways(['normal'])],
     compile_fail, ['-fdiagnostics-show-caret'])
test('NativeTypeCaseRetainedReify', [req_th, only_ways(['normal'])],
     compile_fail, ['-fdiagnostics-show-caret ' + config.ghc_th_way_flags])
test('NativeNestedSumCaptureWrong', normal, compile_fail, [''])
test('NativeIrredIndexEvidence', normal, compile_fail, [''])
test('NativeFamilyIndexEvidence', normal, compile_fail, [''])
test('NativeRetainedSumInference', normal, compile_fail, [''])
test('NativeIndexOperandFamily', normal, compile_fail, [''])
test('NativeRetainedSecondColumn', normal, compile_fail, [''])
test('NativeRetainedSecondGuard', normal, compile_fail, ['-Werror=incomplete-patterns'])
test('NativeRetainedTypeDomain', normal, compile_fail, [''])
test('NativeRetainedTypeWitnessMismatch', normal, compile_fail, [''])
test('NativeRetainedTypeWitnessSynthesis', normal, compile_fail, [''])
test('NativeNestedSumRecursiveAlias', normal, compile_fail, [''])
test('NativeNestedSumFamilyReturn', normal, compile_fail, [''])
