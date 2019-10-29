# difficult to test TH with profiling, because we have to build twice
test('dataToExpQUnit', [omit_ways(prof_ways), req_interp], compile, ['-v0'])
test('T17379', [omit_ways(prof_ways), req_interp], compile_fail, ['-v0'])
test('T17379B', [omit_ways(prof_ways), req_interp], compile_fail, ['-v0'])
