# For slightly unclear reasons this test sometimes performs terribly on arm.
# For the moment we simply give it a slightly higher timeout to address this.
test('jspace', [extra_files(['genJspace']), req_target_smp, req_ghc_smp, js_broken(22573)
               , run_timeout_multiplier(2) ], makefile_test, ['jspace'])
