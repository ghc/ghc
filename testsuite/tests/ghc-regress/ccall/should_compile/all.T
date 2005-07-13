
def ffi( opts ):
  opts.extra_hc_opts = '-fffi -optc-Wno-implicit'

setTestOpts(ffi)

test('cc001', normal, compile, [''])

# Non-static C call
# cc004 test also uses stdcall, so it only works on i386.
if config.platform.startswith('i386-'):
	ways = expect_fail_for(['extcore','optextcore'])
else:
	ways = expect_fail
test('cc004', ways, compile, [''])

# foreign label
test('cc005', expect_fail_for(['extcore','optextcore']), compile, [''])

# Missing:
# test('cc006', normal, compile, [''])

test('cc007', normal, compile, [''])
# foreign label
test('cc008', expect_fail_for(['extcore','optextcore']), compile, [''])
# foreign label
test('cc009', expect_fail_for(['extcore','optextcore']), compile, [''])
# Non-static C call
test('cc010', expect_fail_for(['extcore','optextcore']), compile, [''])
test('cc011', normal, compile, [''])
test('cc012', normal, compile, [''])
test('ffi001', normal, compile, [''])
