test('CountDepsAst', [stat_from_file('deps', 100, 'SIZE'), req_hadrian_deps(["test:count-deps"])], makefile_test, ['count-deps-ast'])
test('CountDepsParser', [stat_from_file('deps', 100, 'SIZE'), req_hadrian_deps(["test:count-deps"])], makefile_test, ['count-deps-parser'])
