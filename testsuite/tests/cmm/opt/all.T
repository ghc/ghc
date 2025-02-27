# Verify that we optimize away conditional branches which always jump
# to the same target.
test('T15188', cmm_src, makefile_test, [])
test('T18141', normal, compile, [''])
test('T20142', normal, compile, [''])

# Cmm opt should not produce oversized literals in the assembly output.
# We check this by telling the assembler to exit on warnings.
test('T24556', [only_ways('optasm'), cmm_src], compile, ['-O -opta -Xassembler -opta --fatal-warnings'])

test('T25771', [cmm_src, only_ways(['optasm']),
                grep_errmsg(r'(12\.345|0\.6640625)',[1]),
                ],
     compile, ['-ddump-cmm-opt'])

