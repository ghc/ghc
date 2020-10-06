test('T18793', [ only_ways(['optasm']), grep_errmsg('Arity=') ], compile, ['-ddump-simpl -dppr-cols=99999 -dsuppress-uniques'])
