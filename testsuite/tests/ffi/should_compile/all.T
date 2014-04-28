
def ffi( name, opts ):
  opts.extra_hc_opts = '-XForeignFunctionInterface -optc-Wno-implicit'

setTestOpts(ffi)

test('cc001', normal, compile, [''])

# Non-static C call
# cc004 test also uses stdcall, so it only works on i386.
if config.platform.startswith('i386-'):
    ways = expect_fail
else:
    ways = normal
test('cc004', ways, compile, [''])

test('cc005', normal, compile, [''])
test('cc007', normal, compile, [''])
test('cc008', normal, compile, [''])
test('cc009', normal, compile, [''])
test('cc010', normal , compile, [''])
test('cc011', normal, compile, [''])
test('cc012', normal, compile, [''])
test('cc013', normal, compile, [''])
test('cc014', normal, compile, [''])
test('ffi-deriv1', normal, compile, [''])
test('T1357', normal, compile, [''])
test('T3624', normal, compile, [''])
test('T3742', normal, compile, [''])
test('cc015', normal, compile, [''])
test('cc016', normal, compile, [''])
