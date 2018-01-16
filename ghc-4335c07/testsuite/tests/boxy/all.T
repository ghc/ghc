# Boxy-type tests

test('Base1',    normal, compile_fail, [''])
test('Church1',  expect_broken(4295), compile, [''])
test('Church2',  expect_broken(1330), compile_fail, [''])
test('PList1',   expect_broken(4295), compile, [''])
test('PList2',   expect_broken(4295), compile, [''])
test('SystemF',  expect_broken(4295), compile, [''])
test('boxy',     expect_broken(4295), compile, [''])
test('Compose',  normal, compile, [''])
test('T2193',    normal, compile_and_run, [''])
