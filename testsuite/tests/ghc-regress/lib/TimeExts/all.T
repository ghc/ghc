# test for obsolete module
setTestOpts(only_compiler_types(['ghc']))

test('timeexts001', normal, compile_and_run, ['-package lang'])
