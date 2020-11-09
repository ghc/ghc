setTestOpts(only_ways(['normal']));

test('qdocompile001', normal, compile, ['-v0 -ddump-rn -dsuppress-uniques'])
test('qdocompile002', normal, compile, ['-v0'])
