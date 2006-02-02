def f( opts ):
  opts.extra_hc_opts = '-fglasgow-exts'

setTestOpts(f)

# Missing:
# test('cc002', normal, compile_fail, [''])

test('cc005', only_compiler_types(['ghc']), compile_fail, [''])

