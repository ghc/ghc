def f( opts ):
  opts.extra_hc_opts = '-fglasgow-exts -package lang'

setTestOpts(f)

test('cc001', normal, compile_fail, [''])

# Missing:
# test('cc002', normal, compile_fail, [''])

test('cc004', normal, compile_fail, [''])
test('cc005', normal, compile_fail, [''])
