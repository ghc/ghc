setTestOpts(only_compiler_types(['ghc']))

test('PolyKinds09', normal, compile_and_run, [''])
test('PolyKinds10', normal, compile_and_run, [''])

test('PolyKinds01', normal, compile, [''])
test('PolyKinds03', normal, compile, [''])
test('PolyKinds05', normal, compile, [''])
test('PolyKinds08', normal, compile, [''])

test('PolyKinds02', normal, compile_fail, [''])
test('PolyKinds04', normal, compile_fail, [''])
test('PolyKinds06', normal, compile_fail, [''])
test('PolyKinds07', normal, compile_fail, [''])
