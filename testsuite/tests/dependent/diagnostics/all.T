test('FamilyArrowArity', normal, compile_fail, [''])
test('StuckFamilyArrowArity', normal, compile_fail, [''])
test('FamilyArrowMatchability', normal, compile_fail, [''])
test('ForallMatchabilityEmpty', normal, compile_fail, [''])
test('ForallMatchabilityTHWrapper', req_th, compile_fail, [''])
test('PartialFamilyArityAfterErasure', req_th, compile_fail, [''])
test('PartialFamilyArityWithSplice', req_th, compile_fail, [''])
test('PatSynMatchableArrow', normal, compile_fail, [''])
test('RetainedQuantificationInKind', normal, compile_fail, [''])
test('DependentArrowMatchability',
     [only_ways(['normal']), extra_files(['DependentArrowProvider.hs'])],
     run_command, ['bash DependentArrowMatchability.sh "{compiler}" "{libdir}"'])
