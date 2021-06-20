test('T6135', normal, compile_and_run, [''])
test('T7689', normal, compile_and_run, [''])
# These tests are using unboxed tuples, so omit ghci
test('T9430', omit_ways(['ghci']), compile_and_run, [''])
test('T4442',
     [omit_ways(['ghci']), when(wordsize(32), expect_broken(15184))],
     compile_and_run, [''])
test('T10481', exit_code(1), compile_and_run, [''])
test('T10678',
     [ collect_stats('bytes allocated',5),
       only_ways(['normal'])
     ],
     compile_and_run, ['-O'])
test('T11296', normal, compile_and_run, [''])
test('T13825-compile', normal, compile_and_run, [''])
test('T16164', normal, compile_and_run, [''])
test('ShowPrim', normal, compile_and_run, [''])
test('T12492', normal, compile_and_run, [''])

# These tests use unboxed tuples, which GHCi doesn't support
test('ArithInt8', omit_ways(['ghci']), compile_and_run, [''])
test('ArithWord8', omit_ways(['ghci']), compile_and_run, [''])
test('ArithInt16', omit_ways(['ghci']), compile_and_run, [''])
test('ArithWord16', omit_ways(['ghci']), compile_and_run, [''])
test('ArithInt32', omit_ways(['ghci']), compile_and_run, [''])
test('ArithWord32', omit_ways(['ghci']), compile_and_run, [''])

test('CmpInt8', normal, compile_and_run, [''])
test('CmpWord8', normal, compile_and_run, [''])
test('CmpInt16', normal, compile_and_run, [''])
test('CmpWord16', normal, compile_and_run, [''])
test('CmpInt32', normal, compile_and_run, [''])
test('CmpWord32', normal, compile_and_run, [''])

test('ShrinkSmallMutableArrayA', normal, compile_and_run, [''])
test('ShrinkSmallMutableArrayB', normal, compile_and_run, [''])
test('T14664', normal, compile_and_run, [''])
test('CStringLength', normal, compile_and_run, ['-O2'])
test('NonNativeSwitch', normal, compile_and_run, ['-O2'])
test('Sized', normal, compile_and_run, [''])
