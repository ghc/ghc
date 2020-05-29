# Impredicativity tests

test('Church2',  expect_broken(1330), compile_fail, [''])

test('PList1',   normal, compile, [''])
test('PList2',   normal, compile, [''])
test('SystemF',  normal, compile, [''])
test('Base1',    normal, compile, [''])
test('Church1',  normal, compile, [''])
test('boxy',     normal, compile, [''])
test('Compose',  normal, compile, [''])
test('T2193',    normal, compile_and_run, [''])
test('icfp20-ok',     normal, compile, [''])
test('icfp20-fail',   normal, compile_fail, [''])
test('T18126-1',      normal, compile, [''])
test('T18126-nasty',  normal, compile, [''])
test('cabal-example', normal, compile, [''])
