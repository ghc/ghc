# This used to be a test for conflicting module names, but the error
# case that it was testing for went away (conflicting module names in
# separate packages are allowed).  Now it is a straightforward test
# for shadowing: the local Foreign module shadows the base package
# one.
test('pkg02', [extra_files(['A.hs', 'Foreign.hs'])], multimod_compile, ['A', '-v0'])
