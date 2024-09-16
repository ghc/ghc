
def ffi( name, opts ):
  opts.extra_hc_opts = '-XForeignFunctionInterface -optc-Wno-implicit'

setTestOpts(ffi)

test('cc001', normal, compile, [''])

test('cc004', expect_fail, compile, [''])

test('cc005', normal, compile, [''])
test('cc007', normal, compile, [''])
test('cc008', normal, compile, [''])
test('cc009', normal, compile, [''])
test('cc010', normal , compile, [''])
test('cc011', normal, compile, [''])
test('cc012', normal, compile, [''])
test('cc013', normal, compile, [''])
test('cc014', normal, compile, [''])
test('ReducingFfiSignature', normal, compile, [''])
test('ffi-deriv1', normal, compile, [''])
test('T1357', normal, compile, [''])
test('T3624', normal, compile, [''])
test('T3742', normal, compile, [''])
test('cc015', normal, compile, [''])
test('cc016', normal, compile, [''])
test('T10460', normal, compile, [''])
test('T11983', normal, compile, ['T11983.c'])
test('T14125', normal, compile, [''])
test(
    'cc017',
    req_th,
    compile,
    [
        '-optc=-DC -optcxx=-DCXX -optcxx=-std=c++11'
        + (' -optcxx=-stdlib=libc++' if opsys('darwin') else '')
    ],
)
test('T15531', normal, compile, ['-Wall'])
test('T22043', normal, compile, [''])

test('T22774', unless(js_arch() or arch('wasm32'), expect_fail), compile, [''])

test('T24034', normal, compile, [''])
test('T25255', normal, compile, ['-dppr-debug'])
