test('drv001', normal, compile, [''])
test('drv002', normal, compile, [''])
test('drv003', normal, compile, [''])
test('drv004', normal, compile, [''])
test('drv005', normal, compile, [''])
test('drv006', normal, compile, [''])
test('drv007', normal, compile, [''])
test('drv008', normal, compile, [''])
test('drv009', normal, compile, [''])
test('drv010', normal, compile, [''])
test('drv011', normal, compile, [''])
test('drv012', normal, compile, [''])
test('drv013', normal, compile, [''])
test('drv014', normal, compile, [''])
test('drv015', normal, compile, [''])
test('drv020', normal, compile, [''])
test('drv021', normal, compile, [''])
test('deriving-1935', normal, compile, [''])
test('T2378', normal, compile, [''])
test('T2856', normal, compile, [''])
test('T3057', if_compiler_lt('ghc', '6.11', expect_fail), compile, [''])
test('T3012', normal, compile, [''])
test('T3965', normal, compile, [''])

test('drv-functor1',
     if_compiler_lt('ghc', '6.11', expect_fail),
     compile,
     [''])
test('drv-functor2', normal, compile, [''])
test('drv-foldable-traversable1',
     if_compiler_lt('ghc', '6.11', expect_fail),
     compile,
     [''])
