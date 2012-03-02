setTestOpts(only_compiler_types(['ghc']))

test('PolyKinds09', normal, compile_and_run, [''])
test('PolyKinds10', normal, compile_and_run, [''])

test('PolyKinds01', normal, compile, [''])
test('PolyKinds03', normal, compile, [''])
test('PolyKinds05', normal, compile, [''])
test('PolyKinds08', normal, compile, [''])
test('PolyKinds11', normal, compile, [''])
test('PolyKinds13', normal, compile, [''])

test('PolyKinds02', normal, compile_fail, [''])
test('PolyKinds04', normal, compile_fail, [''])
test('PolyKinds06', normal, compile_fail, [''])
test('PolyKinds07', normal, compile_fail, [''])

test('PolyKinds12', normal, compile, [''])

test('T5798', normal, compile, [''])
test('T5770', normal, compile, [''])
test('T5771', normal, compile, [''])

test('Freeman', normal, compile_and_run, [''])
test('MonoidsTF', normal, compile_and_run, [''])
test('MonoidsFD', normal, compile_and_run, [''])

