# uses a _casm_, so -fasm is out of the question
test('parser.prog002', omit_ways(['optasm']), multimod_compile, \
	['Read013', '-fvia-C -v0 -fglasgow-exts  -funfold-casms-in-hi-file'])
