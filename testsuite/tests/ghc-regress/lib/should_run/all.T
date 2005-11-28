test('char001', 	normal, compile_and_run, [''])
test('char002', 	normal, compile_and_run, [''])

test('ratio001', 	normal, compile_and_run, [''])

test('rand001',		normal, compile_and_run, [''])
test('read001',		normal, compile_and_run, [''])
test('show001',		normal, compile_and_run, [''])
test('text001',		normal, compile_and_run, [''])

test('tup001',		normal, compile_and_run, [''])

test('addr001',		normal, compile_and_run, [''])
test('dynamic001', 	normal, compile_and_run, [''])
test('dynamic002', 	normal, compile_and_run, [''])
test('enum01', 		normal, compile_and_run, ['-cpp'])
test('enum02', 		normal, compile_and_run, ['-cpp'])
test('enum03', 		normal, compile_and_run, ['-cpp'])
test('enum04', 		expect_fail, compile_and_run, [ ''])
test('exceptions001', 	normal, compile_and_run, [''])
test('exceptions002', 	normal, compile_and_run, [''])
test('list001'	,	normal, compile_and_run, [''])

test('memo001', extra_run_opts('+RTS -A10k -RTS'), \
	compile_and_run, ['-package util'])

test('memo002', extra_run_opts('20'), \
	compile_and_run, ['-package util'])

test('packedstring001', normal, compile_and_run, [''])

test('stableptr001', extra_run_opts('+RTS -K8m -RTS'), compile_and_run, [''])
test('stableptr003', normal, compile_and_run, [''])
test('stableptr004', extra_run_opts('+RTS -K4m -RTS'), compile_and_run, [''])
test('stableptr005', normal, compile_and_run, [''])

test('weak001', normal, compile_and_run, ['-fglasgow-exts'])
