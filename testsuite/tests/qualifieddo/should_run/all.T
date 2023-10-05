setTestOpts(only_ways(['normal']));

qextra_files = extra_files(['Vector.hs', 'Monad'])

test('qdorun001', [qextra_files], multimod_compile_and_run, ['qdorun001', ''])
test('qdorun002', [qextra_files], multimod_compile_and_run, ['qdorun002', ''])
test('qdorun003', [qextra_files], multimod_compile_and_run, ['qdorun003', ''])
test('qdorun004', normal, compile_and_run, [''])
test('qdorun005', [qextra_files], multimod_compile_and_run, ['qdorun005', ''])
test('qdorun006', [req_th,qextra_files], multimod_compile_and_run, ['qdorun006', ''])
test('qdorun007', [qextra_files], multimod_compile_and_run, ['qdorun007', ''])
