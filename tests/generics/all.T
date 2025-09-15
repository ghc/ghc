test('GenCanDoRep0',    normal, compile, [''])
test('GenCanDoRep1',    normal, compile, [''])
test('GenDerivOutput',  normal, compile, ['-dsuppress-uniques'])

test('GenShouldFail0',  normal, compile_fail, [''])
test('GenCannotDoRep0_0', normal, compile_fail, [''])
test('GenCannotDoRep0_1', normal, compile_fail, [''])
test('GenCannotDoRep0_2', normal, compile_fail, [''])

test('GenShouldFail1_0',  normal, compile_fail, [''])
test('GenCannotDoRep1_0', normal, compile_fail, [''])
test('GenCannotDoRep1_1', normal, compile_fail, [''])
test('GenCannotDoRep1_2', normal, compile_fail, [''])
test('GenCannotDoRep1_3', normal, compile_fail, [''])
test('GenCannotDoRep1_4', normal, compile_fail, [''])
test('GenCannotDoRep1_6', normal, compile_fail, [''])
test('GenCannotDoRep1_7', normal, compile_fail, [''])
test('GenCannotDoRep1_8', normal, compile_fail, [''])

test('GFullyStrict', normal, compile_and_run, [''])

test('T5462Yes1', [extra_files(['GEnum/', 'GEq/', 'GFunctor/']),
                   outputdir('out_T5462Yes1'),
                   req_c]
    , multimod_compile_and_run, ['T5462Yes1', '-iGEq -iGEnum -iGFunctor'])
test('T5462Yes2', [extra_files(['GFunctor/']), outputdir('out_T5462Yes2'), req_c]
    , multimod_compile_and_run, ['T5462Yes2', '-iGFunctor'])
test('T5462No1', [extra_files(['GFunctor/']), outputdir('out_T5462No1')]
    , multimod_compile_fail, ['T5462No1', '-iGFunctor'])

test('T5884', [], multimod_compile, ['T5884Other', '-v0'])
test('GenNewtype', normal, compile_and_run, [''])

test('GenDerivOutput1_0', normal, compile, ['-dsuppress-uniques'])
test('GenDerivOutput1_1', normal, compile, ['-dsuppress-uniques'])

test('T7878', [], multimod_compile, ['T7878', '-v0'])

test('T8468', normal, compile_fail, [''])
test('T8479', normal, compile, [''])
test('T9563', normal, compile, [''])
test('T10030', normal, compile_and_run, [''])
test('T10361a', normal, compile, [''])
test('T10361b', normal, compile, [''])
test('T11358', normal, compile_and_run, [''])
test('T12220', normal, compile, [''])
test('T15012', [extra_files(['T15012.hs', 'T15012a.hs'])], makefile_test, [])
test('T14266', normal, compile, [''])
test('T19819', normal, compile_and_run, [''])
test('T21185', normal, compile, [''])
test('T25148a', normal, compile, [''])
test('T25148b', normal, compile, [''])
