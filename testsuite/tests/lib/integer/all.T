test('integerNegate', normal, compile, ['-ddump-simpl -dno-typeable-binds -dsuppress-all -dsuppress-uniques'])
test('integerBits', normal, compile_and_run, [''])
test('integerConversions', normal, compile_and_run, [''])
test('plusMinusInteger', [omit_ways(['ghci'])], compile_and_run, [''])
test('integerConstantFolding', normal, makefile_test, ['integerConstantFolding'])
test('naturalConstantFolding', normal, makefile_test, ['naturalConstantFolding'])

test('fromToInteger', normal, makefile_test, ['fromToInteger'])

test('IntegerConversionRules', [], makefile_test, ['IntegerConversionRules'])
test('gcdInteger', normal, compile_and_run, [''])
test('gcdeInteger', normal, compile_and_run, [''])
test('integerPowMod', [], compile_and_run, [''])
test('integerGcdExt', [], compile_and_run, [''])
test('integerRecipMod', [], compile_and_run, [''])
test('bignumMatch', [], compile, [''])
test('T18813', [], compile_and_run, [''])
test('T19170', [], compile_and_run, [''])

# skipped on Windows (doesn't support `-dynamic-too`)
test('T19264', [when(opsys('mingw32'),skip),extra_files(['T19264b.hs'])], compile, ['-dynamic-too --make -v0'])

# skip ghci as it doesn't support unboxed tuples
test('integerImportExport', [omit_ways(['ghci'])], compile_and_run, [''])

# Disable GMP only tests
#test('integerGmpInternals', [], compile_and_run, [''])

test('T19345', [], compile_and_run, [''])
test('T20066', [exit_code(1)], compile_and_run, [''])
