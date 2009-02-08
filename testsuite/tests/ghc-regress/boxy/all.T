# Boxy-type tests

test('Base1',    if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('Church1',  normal, compile, [''])
test('Church2',  expect_broken(1330), compile_fail, [''])
test('PList1',   if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('PList2',   if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('SystemF',  normal, compile, [''])
test('boxy',     if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('Compose',  if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
