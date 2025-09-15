# should_compile_noflag_nohaddock
#
# * noflag: we do *not* pass the -haddock flag
#
# * nohaddock: and tests do *not* contain haddock annotations
#
# When adding a new test here, think about adding it to the
# should_compile_flag_nohaddock directory as well.

test('haddockD001', normal, compile, [''])
test('haddockD002', normal, compile, [''])
test('haddockD003', normal, compile, [''])
test('haddockD004', normal, compile, [''])
test('haddockD005', normal, compile, ['-haddock -no-haddock'])
