# Verify that we optimize away conditional branches which always jump
# to the same target.
test('T15188', cmm_src, makefile_test, [])
test('T18141', normal, compile, [''])
test('T20142', normal, compile, [''])

test('T25771', [cmm_src, only_ways(['optasm']),
                grep_errmsg(r'(12\.345|0\.6640625)',[1]),
                ],
     compile, ['-ddump-cmm'])
