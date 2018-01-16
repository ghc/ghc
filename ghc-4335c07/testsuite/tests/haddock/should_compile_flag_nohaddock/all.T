# should_compile_flag_nohaddock
#
# * flag: we *do* pass the -haddock flag
#
# * nohaddock: but tests do *not* contain haddock annotations
#
# When adding a new test here, think about adding it to the
# should_compile_noflag_nohaddock directory as well.

test('haddockB001', normal, compile, ['-haddock'])
test('haddockB002', normal, compile, ['-haddock'])
test('haddockB003', normal, compile, ['-haddock'])
test('haddockB004', normal, compile, ['-haddock'])
