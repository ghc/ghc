
def f( opts ):
  opts.compile_to_hc = 1
  opts.omit_ways = ['optasm']
  opts.extra_hc_opts = '-fglasgow-exts'

setTestOpts(f)

test('cc001', normal, compile, [''])
test('cc002', normal, compile, [''])
test('cc003', normal, compile, [''])
test('cc004', normal, compile, [''])
test('cc005', normal, compile, [''])

# Missing:
# test('cc006', normal, compile, [''])

test('cc007', normal, compile, [''])
test('cc008', normal, compile, [''])
test('cc009', normal, compile, [''])
test('cc010', normal, compile, [''])
test('cc011', normal, compile, [''])
test('cc012', normal, compile, [''])
