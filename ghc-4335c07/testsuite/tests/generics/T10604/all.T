test('T10604_Authenticated',           normal, compile,      [''])
test('T10604_deriving',                normal, compile,      ['-dsuppress-uniques'])
test('T10604_bad_variable_occurrence', normal, compile_fail, [''])
test('T10604_no_PolyKinds',            normal, compile_fail, [''])
