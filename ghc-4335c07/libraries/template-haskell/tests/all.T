# difficult to test TH with profiling, because we have to build twice
test('dataToExpQUnit', omit_ways(prof_ways), compile, ['-v0'])
