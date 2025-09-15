test('T11173', [], multimod_compile, ['T11173', '-v0'])
test('T12609', normal, compile, [''])
test('T16597', [], multimod_compile, ['T16597', '-v0'])
test('T17176', normal, compile, [''])
test('DRFPatSynExport', [], makefile_test, ['DRFPatSynExport'])
test('NoFieldSelectors', req_th, compile, [''])
test('NFSDRF', normal, compile, [''])
test('NFSImport', [extra_files(['NFSExport.hs'])], multimod_compile, ['NFSImport NFSExport', '-v0'])
test('T18999_NoFieldSelectors', normal, compile, [''])
test('T18999_FieldSelectors', normal, compile, [''])
test('T19154', normal, compile, [''])
test('T20723', normal, compile, [''])
test('T20989', normal, compile, [''])
test('T21625', [], multimod_compile, ['T21625', '-v0'])
test('DupFldFixity1', normal, compile, [''])
test('DupFldFixity2', normal, compile, [''])
test('T23010', [extra_files(['T23010.hs-boot', 'T23010_aux.hs'])]
             , multimod_compile
             , ['T23010', '-v0'])
test('T14848', req_th, compile, [''])
test('T17551', req_th, compile, [''])
test('T11103', req_th, compile, [''])
test('T13352'
    , [extra_files(['T13352_A.hs', 'T13352_B.hs'])]
    , multimod_compile, ['T13352_A T13352_B T13352', '-v0'])
test('T13352_hard'
    , [extra_files(['T13352_hard_A.hs', 'T13352_hard_B.hs'])]
    , multimod_compile_fail, ['T13352_hard_A T13352_hard_B T13352_hard', '-v0'])
test('T21720', req_th, compile, [''])
test('T21898', normal, compile, [''])
test('T22160', [extra_files(['T22160_A.hs', 'T22160_B.hs', 'T22160_C.hs'])]
             , multimod_compile, ['T22160_A T22160_B T22160_C T22160', '-v0'])
test('DupFldFixity3', normal, compile, [''])
test('overloadedrecflds10'
    , [extra_files(['OverloadedRecFlds10_A.hs', 'OverloadedRecFlds10_B.hs', 'OverloadedRecFlds10_C.hs'])]
    , multimod_compile
    , ['overloadedrecflds10', '-v0'])
test('NoDRFModuleExport'
    , [extra_files(['NoDRFModuleExport_aux.hs'])]
    , multimod_compile
    , ['NoDRFModuleExport', '-v0'])
test('BootFldReexport'
    , [extra_files([ 'BootFldReexport_N.hs', 'BootFldReexport_N.hs-boot'
                   , 'BootFldReexport_O.hs', 'BootFldReexport_O.hs-boot'
                   , 'BootFldReexport_B.hs' ])]
    , multimod_compile_fail
      # Should either pass or give an ambiguity error when compiling
      # the final module (BootFldReexport), but not fail earlier.
    , ['BootFldReexport', '-v0'])
test('T23220'
    , [req_th, extra_files(['T23220_aux.hs']), js_broken(24886)]
    , multimod_compile, ['T23220_aux.hs T23220.hs', '-v0'])
test('T22106_A', [extra_files(['T22106_aux.hs'])], multimod_compile, ['T22106_A', '-v0'])
test('T22106_B', [extra_files(['T22106_aux.hs'])], multimod_compile, ['T22106_B', '-v0'])
test('T22106_C', [extra_files(['T22106_aux.hs'])], multimod_compile_fail, ['T22106_C', '-v0'])
test('T22106_D', [extra_files(['T22106_aux.hs'])], multimod_compile, ['T22106_D', '-v0'])
test('T22424', req_th, compile, ['-this-unit-id="me"'])
test('T23279', [extra_files(['T23279_aux.hs'])], multimod_compile, ['T23279', '-v0'])
test('T23557', [extra_files(['T23557_aux.hs'])], multimod_compile, ['T23557', '-v0'])
test('T24293', req_th, compile, [''])
test('T24293b', req_th, compile, [''])
test('T24293c', req_th, compile_fail, [''])
test('T24381', normal, compile, [''])
