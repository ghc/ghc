#
test('selfloop', [cmm_src], compile, ['-no-hs-main'])
test('cmm_sink_sp', [ when(wordsize(32), skip), only_ways(['optasm']), grep_errmsg('(\[Sp.*\]).*(=).*(\[.*R1.*\]).*;',[1,2,3]), cmm_src], compile, ['-no-hs-main -ddump-cmm -dsuppress-uniques -O -dsuppress-all'])
test('cmm_sink_hp', [when(wordsize(32), skip), cmm_src,grep_errmsg('(I64\[.*Hp.*=.*;)',[1])], compile, ['-no-hs-main -O -ddump-cmm -fcmm-sink-alias'])
test('cmm_hp_aliases', [ when(wordsize(32), skip), cmm_src,grep_errmsg('(I64\[.*Hp.*=.*;)',[1])], compile, ['-no-hs-main -O -ddump-cmm -fcmm-sink-alias'])

test('T16930', normal, makefile_test, ['T16930'])
test('T17442', normal, compile, [''])
test('T20725', normal, compile, ['-package ghc'])
