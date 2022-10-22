
## Correctness tests: these programs should compile and pass Core Lint.
test('DCo_Array', [extra_files(['DCo_Array_aux.hs'])], multimod_compile, ['DCo_Array.hs', '-v0 -O'])
test('DCo_Coercion', normal, compile, ['-O'])
test('DCo_Hetero', normal, compile, [''])
test('DCo_HsBinds', normal, compile, ['-O'])
test('DCo_HsType', normal, compile, [''])
test('DCo_InScope', normal, compile, [''])
test('DCo_LiftTyped', normal, compile, [''])
test('DCo_Phantom', normal, compile, ['-fdefer-type-errors'])
test('DCo_PostProcess', normal, compile, [''])
test('DCo_Specialise', normal, compile, [''])
test('DCo_T15703_aux', normal, compile, ['-O'])
test('DCo_Typeable', normal, compile, ['-O'])
test('DCo_TypeRep', normal, compile, [''])

## The following tests that we optimise away certain transitive coercions.
## However, this optimisation was too slow, so we comment this out for now.
#test('DCo_TransOpt', normal, compile_and_run, ['-package ghc'])
