# uses a _casm_, so -fasm is out of the question
# non-static C call
test('parser.prog002', compose(expect_fail_for(['extcore','optextcore']),omit_ways(['optasm'])), multimod_compile, \
	['Read013', '-fvia-C -v0 -fglasgow-exts  -funfold-casms-in-hi-file'])
