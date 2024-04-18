setTestOpts(only_ways(['normal']));

test('qdocompile001', normal, compile, ['-v0 -ddump-rn -dsuppress-uniques'])
test('qdocompile002', normal, compile, ['-v0'])
test('T8671a', normal, compile, ['--make -v0 -Wall -Wunused-do-bind'])
test('T8671b', normal, compile, ['--make -v0 -Wall -Wunused-do-bind'])