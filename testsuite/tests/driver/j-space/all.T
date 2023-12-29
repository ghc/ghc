test('jspace', [extra_files(['genJspace']), req_target_smp, req_ghc_smp, js_broken(22573)], makefile_test, ['jspace'])
