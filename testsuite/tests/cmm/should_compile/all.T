setTestOpts(
  [ js_skip # Cmm not supported by the JS backend
  ])

test('selfloop', [cmm_src], compile, ['-no-hs-main'])
test('cmm_sink_sp', [ only_ways(['optasm']),
                      when(arch('wasm32'), fragile(24152)),
                      grep_errmsg(r'(\[Sp.*\]).*(=).*(\[.*R1.*\]).*;',[1,2,3]),
                      cmm_src], compile, ['-no-hs-main -ddump-cmm -dsuppress-uniques -O'])

test('T16930', normal, makefile_test, ['T16930'])
test('T17442', normal, compile, [''])
test('T20725', normal, compile, ['-package ghc'])
test('T23610', normal, makefile_test, ['T23610'])
