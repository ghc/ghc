test('parser_unit_tests',
     normal,
     compile_and_run,
     ['-package ghc'])

test('readRun001', normal, compile_and_run, [''])
test('readRun002', normal, compile_and_run, [''])
test('readRun003', normal, compile_and_run, [''])
test('readRun004', normal, compile_and_run, ['-fobject-code'])
test('T1344', normal, compile_and_run, [''])
test('operator', normal, compile_and_run, [''])
test('operator2', normal, compile_and_run, [''])
test('ParserMultiWayIf', [], compile_and_run, [''])
test('BinaryLiterals0', normal, compile_and_run, [''])
test('BinaryLiterals1', [], compile_and_run, [''])
test('BinaryLiterals2', [], compile_and_run, [''])
test('T10807', normal, compile_and_run, [''])
test('NegativeZero', normal, compile_and_run, [''])
test('HexFloatLiterals', normal, compile_and_run, [''])
test('NumericUnderscores0', normal, compile_and_run, [''])
test('NumericUnderscores1', normal, compile_and_run, [''])
test('LexNegLit', normal, compile_and_run, [''])
test('RecordDotSyntax1', normal, compile_and_run, [''])
test('RecordDotSyntax2', normal, compile_and_run, [''])
test('RecordDotSyntax3', [extra_files(['RecordDotSyntaxA.hs'])], multimod_compile_and_run, ['RecordDotSyntax3', ''])
test('RecordDotSyntax4', [extra_files(['RecordDotSyntaxA.hs'])], multimod_compile_and_run, ['RecordDotSyntax4', ''])
test('RecordDotSyntax5', normal, compile_and_run, [''])
test('ListTuplePunsConstraints', extra_files(['ListTuplePunsConstraints.hs']), ghci_script, ['ListTuplePunsConstraints.script'])
test('T25937', normal, compile_and_run, [''])

# Multiline strings
test('MultilineStrings', normal, compile_and_run, [''])
test('MultilineStringsOverloaded', normal, compile_and_run, [''])
test('T25375', normal, compile_and_run, [''])
test('T25609', normal, compile_and_run, [''])
test('T25784', normal, compile_and_run, [''])

# String interpolation
test('StringInterpolation', normal, compile_and_run, [''])
test('StringInterpolationOverloaded', normal, compile_and_run, [''])
