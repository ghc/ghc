setTestOpts(only_ways(['normal']));

test('mdofail001', normal, compile_fail, [''])
test('mdofail002', normal, compile_fail, [''])
test('mdofail003', normal, compile_fail, [''])
test('mdofail004', normal, compile, [''])
test('mdofail005', normal, compile_fail, [''])
test('mdofail006', exit_code(1), compile_and_run, [''])
