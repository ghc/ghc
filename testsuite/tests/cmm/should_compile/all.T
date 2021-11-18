#
test('selfloop', [cmm_src], compile, ['-no-hs-main'])
test('T16930', normal, makefile_test, ['T16930'])
test('cmm_weak', normal, makefile_test, ['cmm_weak'])
test('T17442', normal, compile, [''])
