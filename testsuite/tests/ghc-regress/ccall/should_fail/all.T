def f( opts ):
  opts.extra_hc_opts = '-fglasgow-exts'

setTestOpts(f)

# Missing:
# test('cc002', normal, compile_fail, [''])

test('cc005', only_compiler_types(['ghc']), compile_fail, [''])
test('ccfail001', only_compiler_types(['ghc']), compile_fail, [''])
test('ccfail002', only_compiler_types(['ghc']), compile_fail, [''])
