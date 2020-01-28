test('T16282', [check_errmsg('-Wall-missed-specialisations')], multimod_compile, ['T16282', '-O2 -Wall-missed-specialisations'])
