# difficult to test TH with profiling, because we have to build twice
test('dataToExpQUnit', [omit_ways(prof_ways), req_th], compile, ['-v0'])
test('dataToCodeQUnit', [omit_ways(prof_ways), req_th], compile, ['-v0'])
test('pragCompletePpr', [omit_ways(prof_ways), req_th], compile_and_run, [''])
