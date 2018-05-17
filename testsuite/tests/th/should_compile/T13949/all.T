# we omit profasm because it doesn't bring much to the table but
# introduces its share of complexity, as the test as it is fails with
# profasm:
# ASCII.hs:1:1: fatal:
#     Cannot load -prof objects when GHC is built with -dynamic
#     To fix this, either:
#       (1) Use -fexternal-interpreter, or
#       (2) Build the program twice: once with -dynamic, and then
#           with -prof using -osuf to set a different object file suffix.
test('T13949', [extra_files(['ASCII.hs', 'PatternGenerator.hs', 'These.hs', 'Tree.hs']),
	        omit_ways(['profasm'])],
     multimod_compile, ['ASCII PatternGenerator These Tree', '-fno-code -v0'])