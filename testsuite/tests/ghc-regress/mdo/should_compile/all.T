setTestOpts(only_ways(['normal']));

test('mdo001', normal, compile_and_run, [''])
test('mdo002', normal, compile_and_run, [''])
test('mdo003', normal, compile_and_run, [''])
test('mdo004', only_compiler_types(['ghc']), compile_and_run, [''])
test('mdo005', normal, compile_and_run, [''])
test('mdo006', normal, compile, [''])
