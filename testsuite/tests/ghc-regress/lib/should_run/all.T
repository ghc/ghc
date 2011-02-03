test('char001', 	normal, compile_and_run, [''])
test('char002', 	normal, compile_and_run, [''])

test('length001',
     # This fails without -O, as it relies on a RULE being applied
     expect_fail_for(['normal', 'threaded1', 'llvm']),
     compile_and_run,
     [''])

test('ratio001', 	normal, compile_and_run, [''])

test('rand001',		normal, compile_and_run, [''])
test('reads001',		normal, compile_and_run, [''])
test('show001',		normal, compile_and_run, [''])
test('text001',		normal, compile_and_run, [''])

test('tup001',		normal, compile_and_run, [''])

test('addr001',		normal, compile_and_run, [''])
test('dynamic001', 	skip_if_fast, compile_and_run, [''])
test('dynamic002', 	[skip_if_fast,reqlib('packedstring')], compile_and_run, ['-package packedstring'])
test('enum01', 		skip_if_fast, compile_and_run, ['-cpp'])
test('enum02', 		skip_if_fast, compile_and_run, ['-cpp'])
test('enum03', 		skip_if_fast, compile_and_run, ['-cpp'])
test('enum04', 		normal, compile_and_run, [''])
test('exceptionsrun001', 	normal, compile_and_run, [''])
test('exceptionsrun002', 	normal, compile_and_run, [''])
test('list001'	,	skip_if_fast, compile_and_run, [''])
test('list002', skip_if_fast, compile_and_run, [''])
test('list003', skip_if_fast, compile_and_run, [''])

test('memo001',
     [skip_if_fast,
      extra_run_opts('+RTS -A10k -RTS'),
      extra_clean(['Memo1.hi', 'Memo1.o'])],
     multimod_compile_and_run,
     ['memo001',''])

test('memo002',
     [skip_if_fast,
      extra_run_opts('20'),
      extra_clean(['Memo2.hi', 'Memo2.o'])],
	multimod_compile_and_run, ['memo002',''])

test('packedstring001', reqlib('packedstring'), compile_and_run, ['-package packedstring'])

test('stableptr001',
     [skip_if_fast, extra_run_opts('+RTS -K8m -RTS')],
     compile_and_run, [''])
test('stableptr003', normal, compile_and_run, [''])
test('stableptr004', extra_run_opts('+RTS -K4m -RTS'), compile_and_run, [''])
test('stableptr005', normal, compile_and_run, [''])

test('weak001', normal, compile_and_run, [''])
