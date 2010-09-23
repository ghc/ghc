# Boxy-type tests

test('Base1',    expect_broken(4295), compile, [''])
test('Church1',  expect_broken(4295), compile, [''])
test('Church2',  expect_broken(1330), compile_fail, [''])
test('PList1',   expect_broken(4295), compile, [''])
test('PList2',   expect_broken(4295), compile, [''])
test('SystemF',  expect_broken(4295), compile, [''])
test('boxy',     expect_broken(4295), compile, [''])
test('Compose',  if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('T2193',  if_compiler_lt('ghc', '6.11', expect_fail), compile_and_run, [''])
