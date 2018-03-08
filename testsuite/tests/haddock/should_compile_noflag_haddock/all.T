# should_compile_noflag_haddock
#
# * noflag: we do *not* pass the -haddock flag
#
# * haddock: but tests *do* contain haddock annotations
#
# When adding a new test here, think about adding it to the
# should_compile_flag_haddock directory as well.

test('haddockC001', normal, compile, [''])
test('haddockC002', normal, compile, [''])
test('haddockC003', normal, compile, [''])
test('haddockC004', normal, compile, [''])
test('haddockC005', normal, compile, [''])
test('haddockC006', normal, compile, [''])
test('haddockC007', normal, compile, [''])
test('haddockC008', normal, compile, [''])
test('haddockC009', normal, compile, [''])
test('haddockC010', normal, compile, [''])
test('haddockC011', normal, compile, [''])
test('haddockC012', normal, compile, [''])
test('haddockC013', normal, compile, [''])
test('haddockC014', normal, compile, [''])
test('haddockC015', normal, compile, [''])
test('haddockC016', normal, compile, [''])
test('haddockC017', normal, compile, [''])
test('haddockC018', normal, compile, [''])
test('haddockC019', normal, compile, [''])
test('haddockC020', normal, compile, [''])
test('haddockC021', normal, compile, [''])
test('haddockC022', normal, compile, [''])
test('haddockC023', normal, compile, [''])
test('haddockC024', normal, compile, [''])
test('haddockC025', normal, compile, [''])
test('haddockC026', normal, compile, ['-XRankNTypes'])
test('haddockC027', normal, compile, ['-XRankNTypes'])
test('haddockC028', normal, compile, ['-XTypeOperators'])
test('haddockC029', normal, compile, [''])
test('haddockC030', normal, compile, [''])
test('haddockC031', normal, compile, ['-XExistentialQuantification'])
test('haddockC032', normal, compile, [''])
test('haddockC035', normal, compile, [''])
test('haddockC036', normal, compile, [''])
test('haddockC037', normal, compile, [''])

# The tests below this line are not duplicated in
# should_compile_flag_haddock.

test('haddockSimplUtilsBug', normal, compile, [''])
