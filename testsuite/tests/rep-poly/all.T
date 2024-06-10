
test('T11473', normal, compile_fail, [''])
test('T11724', normal, compile_fail, [''])
test('T12709', normal, compile_fail, [''])
test('T12973', normal, compile_fail, [''])
test('T13233', normal, compile_fail, [''])
test('T13929', normal, compile_fail, [''])
test('T14561', normal, compile_fail, [''])
test('T14561b', normal, compile_fail, [''])
test('T14765', normal, compile_fail, [''])
test('T17360', normal, compile_fail, [''])
test('T17817', normal, compile_fail, [''])
test('T18170a', [req_th,extra_files(['T18170c.hs'])], multimod_compile, ['T18170a.hs', '-v0'])
test('T18170b', [extra_files(['T18170c.hs']), expect_broken(19893)], multimod_compile_fail, ['T18170b.hs', '-v0'])
# T18170b isn't actually broken, but it causes a Core Lint error
# even though the program is (correctly) rejected by the typechecker
test('T18481', normal, compile, [''])
test('T18481a', normal, compile, [''])
test('T18534', normal, compile_fail, [''])
test('T19615', normal, compile_fail, [''])
test('T19709a', normal, compile_fail, [''])
test('T19709b', normal, compile_fail, [''])
test('T20113', normal, compile_fail, [''])
test('T20277', normal, compile_fail, [''])
test('T20330a', normal, compile, [''])
test('T20330b', normal, compile, [''])
test('T20423', normal, compile_fail, [''])
test('T20423b', normal, compile_fail, [''])
test('T20426', normal, compile_fail, [''])
test('T21239', normal, compile, [''])
test('T21544', normal, compile, ['-Wno-deprecated-flags'])
test('T21906', normal, compile_fail, [''])
test('T22291', normal, compile, [''])
test('T22291b', normal, compile, [''])
test('T23051', normal, compile_fail, [''])
test('T23153', normal, compile_fail, [''])
test('T23154', normal, compile_fail, [''])
test('T23176', normal, compile_fail, ['-XPartialTypeSignatures -fdefer-out-of-scope-variables'])
test('T23903', normal, compile_fail, [''])
test('T23505', normal, compile_fail, [''])

test('EtaExpandDataCon', normal, compile, ['-O'])
test('EtaExpandStupid1', normal, compile, ['-Wno-deprecated-flags'])
test('EtaExpandStupid2', normal, compile_fail, ['-Wno-deprecated-flags'])
test('LevPolyDataToTag1', normal, compile, [''])
test('LevPolyDataToTag2', normal, compile_fail, [''])
test('LevPolyLet', normal, compile_fail, [''])
test('PandocArrowCmd', normal, compile, [''])
test('RepPolyApp', normal, compile_fail, [''])
test('RepPolyArgument', normal, compile_fail, [''])
test('RepPolyArrowCmd', normal, compile_fail, [''])
test('RepPolyArrowFun', normal, compile_fail, [''])
test('RepPolyBackpack1', normal, backpack_compile_fail, [''])
test('RepPolyBackpack2', req_c, backpack_run, [''])
test('RepPolyBackpack3', normal, backpack_compile_fail, [''])
test('RepPolyBackpack4', req_c, backpack_run, [''])
test('RepPolyBackpack5', js_broken(22361), backpack_run, [''])
test('RepPolyBinder', normal, compile_fail, [''])
test('RepPolyCase1', normal, compile_fail, [''])
test('RepPolyClassMethod', normal, compile_fail, [''])
test('RepPolyDeferred', normal, compile_fail, [''])
test('RepPolyDoBind', normal, compile_fail, [''])
test('RepPolyDoBody1', normal, compile_fail, [''])
test('RepPolyDoBody2', normal, compile_fail, [''])
test('RepPolyDoReturn', normal, compile, [''])
test('RepPolyFFI', normal, compile, [''])
test('RepPolyLambda', normal, compile_fail, [''])
test('RepPolyInferPatBind', normal, compile_fail, [''])
test('RepPolyInferPatSyn', normal, compile_fail, [''])
test('RepPolyLeftSection1', normal, compile, [''])
test('RepPolyLeftSection2', normal, compile_fail, [''])
test('RepPolyMagic', normal, compile_fail, [''])
test('RepPolyMatch', normal, compile_fail, [''])
test('RepPolyMcBind', normal, compile_fail, [''])
test('RepPolyMcBody', normal, compile_fail, [''])
test('RepPolyMcGuard', normal, compile_fail, [''])
test('RepPolyNewtypePat1', normal, compile_fail, [''])
test('RepPolyNewtypePat2', normal, compile, [''])
test('RepPolyNPlusK', normal, compile_fail, [''])
test('RepPolyPatBind', normal, compile_fail, [''])
test('RepPolyPatSynArg', normal, compile_fail, [''])
test('RepPolyPatSynRes', normal, compile_fail, [''])
test('RepPolyPatSynUnliftedNewtype', normal, compile_fail, [''])
test('RepPolyRecordPattern', normal, compile_fail, [''])
test('RepPolyRecordUpdate', normal, compile_fail, [''])
test('RepPolyRightSection', normal, compile_fail, [''])
test('RepPolyRule1', normal, compile_fail, [''])
test('RepPolyRule2', normal, compile_fail, [''])
test('RepPolySum', normal, compile_fail, [''])
test('RepPolyTuple', normal, compile_fail, [''])
test('RepPolyTuple4', normal, compile_fail, [''])
test('RepPolyTupleSection', normal, compile_fail, [''])
test('RepPolyUnboxedPatterns', normal, compile_fail, [''])
test('RepPolyUnliftedDatatype', normal, compile, [''])
test('RepPolyUnliftedDatatype2', normal, compile, ['-O'])
test('RepPolyUnliftedNewtype', normal, compile,
     ['-fno-warn-partial-type-signatures -fno-warn-deprecated-flags'])
# js_skip: RepPolyUnsafeCoerce1 coerces from UnliftedRep to an unknown rep r
# for which the JS backend can't compute a PrimRep.
# Came up in https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10088#note_499218
# (or, rather when submitting to CI)
test('RepPolyUnsafeCoerce1', [js_skip], compile, [''])
test('RepPolyWildcardPattern', normal, compile_fail, [''])
test('RepPolyWrappedVar', normal, compile_fail, [''])
# js_skip: RepPolyWrappedVar2 coerces from AddrRep to IntRep, which have
# different PrimReps in JS.
# Came up in https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10088#note_499218
test('RepPolyWrappedVar2', [js_skip], compile, [''])
test('UnliftedNewtypesCoerceFail', normal, compile_fail, [''])
test('UnliftedNewtypesLevityBinder', normal, compile_fail, [''])
test('T24769a', normal, compile_fail, [''])
test('T24769b', normal, compile_fail, [''])
test('T24769c', [extra_files(['T24769c_aux.hs'])], multimod_compile_fail, ['T24769c', ''])
test('T24769d', normal, compile_fail, [''])
test('T24769e', normal, ghci_script, ['T24769e.script'])

###############################################################################
## The following tests require rewriting in RuntimeReps,                     ##
## i.e. PHASE 2 of the FixedRuntimeRep plan.                                 ##
##                                                                           ##
## These tests work!                                                         ##
                                                                             ##
test('T13105', js_broken(22364), compile, [''])                              ##
test('T17536b', normal, compile, [''])                                       ##
test('RepPolyArgument2', normal, compile, [''])                              ##
test('RepPolyTuple2', normal, compile_and_run, [''])                         ##
test('RepPolyTuple2b', normal, compile_and_run, [''])                        ##
test('RepPolyTuple3', normal, compile_fail, [''])                            ##
test('RepPolyUnsafeCoerce2', normal, compile, [''])                          ##
                                                                             ##
## These don't!                                                              ##
## For the moment, we check that we get the expected error message,          ##
## as we want to reject these in the typechecker instead of getting          ##
## a compiler crash.                                                         ##
                                                                             ##
test('T17021', normal, compile_fail, [''])                                   ##
test('T20363', normal, compile_fail, [''])                                   ##
test('T20363_show_co', normal, compile_fail, ['-fprint-explicit-coercions']) ##
test('T20363b', normal, compile_fail, [''])                                  ##
test('RepPolyCase2', normal, compile_fail, [''])                             ##
test('RepPolyRule3', normal, compile_fail, [''])                             ##
test('T21650_a', normal, compile_fail, ['-Wno-deprecated-flags'])            ##
test('T21650_b', normal, compile_fail, ['-Wno-deprecated-flags'])            ##
###############################################################################
