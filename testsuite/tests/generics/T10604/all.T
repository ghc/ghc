test('T10604_Authenticated',           normal, compile,      [''])
test('T10604_deriving',                [normalise_ddump_deriv], compile, ['-ddump-deriv -dsuppress-uniques'])
test('T10604_bad_variable_occurrence', normal, compile_fail, [''])
test('T10604_no_PolyKinds',            normal, compile_fail, [''])
