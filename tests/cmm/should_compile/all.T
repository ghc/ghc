setTestOpts(
  [ req_cmm
  ])

test('selfloop', [cmm_src], compile, ['-no-hs-main'])
test('cmm_sink_sp', [ only_ways(['optasm']),
                      grep_errmsg(r'(\[Sp.*\]).*(=).*(\[.*R1.*\]).*;',[1,2,3]),
                      cmm_src], compile, ['-no-hs-main -ddump-cmm -dsuppress-uniques -O'])

test('T16930', normal, makefile_test, ['T16930'])
test('T17442', normal, compile, [''])
test('T20725', normal, compile, ['-package ghc'])
test('T23610', normal, makefile_test, ['T23610'])
test('T24224', [cmm_src, grep_errmsg(r'(F64.*);', [1])], compile, ['-no-hs-main -ddump-cmm -dsuppress-all -dsuppress-uniques'])
test('T24474', cmm_src, compile, ['-optc-g3'])
test('T24474-cmm-gets-c-opts', cmm_src, compile, ['-optc-DFOO'])
test('T24474-cmm-opt-order', cmm_src, compile, ['-optc-DFOO '
                                                '-optCmmP-UFOO '
                                                '-optCmmP-UBAR '
                                                '-optc-DBAR'])
