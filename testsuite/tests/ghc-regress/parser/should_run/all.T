test('read001', normal, compile_and_run, [''])
# also make sure that a mere -fglasgow-exts doesn't break
# the default/standard behavior:
test('read001', only_compiler_types(['ghc']), compile_and_run, ['-fglasgow-exts'])
test('read002', normal, compile_and_run, [''])
test('read003', normal, compile_and_run, [''])
