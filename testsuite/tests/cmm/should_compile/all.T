#
test('selfloop', [cmm_src], compile, ['-no-hs-main'])
test('cmm_sink_sp', [ only_ways(['optasm']), grep_errmsg('I64\[Sp.*\].*=.*\[.*R1.*\].*;',True), cmm_src], compile, ['-no-hs-main -ddump-cmm -dsuppress-uniques -O'])

test('T16930', normal, makefile_test, ['T16930'])
test('T17442', normal, compile, [''])
