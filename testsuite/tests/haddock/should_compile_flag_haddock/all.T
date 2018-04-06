# should_compile_flag_haddock.
#
# * flag: we *do* pass the -haddock flag
#
# * haddock: and tests *do* contain haddock annotations
#
# When adding a new test here, think about adding it to the
# should_compile_noflag_haddock directory as well.

test('haddockA001', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA002', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA003', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA004', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA005', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA006', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA007', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA008', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA009', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA010', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA011', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA012', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA013', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA014', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA015', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA016', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA017', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA018', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA019', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA020', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA021', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA022', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA023', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA024', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA025', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA026', normal, compile, ['-haddock -ddump-parsed -XRankNTypes'])
test('haddockA027', normal, compile, ['-haddock -ddump-parsed -XRankNTypes'])
test('haddockA028', normal, compile, ['-haddock -ddump-parsed -XTypeOperators'])
test('haddockA029', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA030', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA031', normal, compile, ['-haddock -ddump-parsed -XExistentialQuantification'])
test('haddockA032', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA035', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA036', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA037', normal, compile, ['-haddock -ddump-parsed'])

# The tests below this line are not duplicated in
# should_compile_noflag_haddock.

test('haddockA033', normal, compile, ['-haddock -ddump-parsed'])
test('haddockA034', normal, compile, ['-haddock -ddump-parsed'])
test('T10398', normal, compile, ['-haddock -ddump-parsed'])
test('T11768', normal, compile, ['-haddock -ddump-parsed'])
