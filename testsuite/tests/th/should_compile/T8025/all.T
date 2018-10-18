# we omit profasm because it fails with:
# B.hs:5:5: fatal:
#     Cannot load -prof objects when GHC is built with -dynamic
#     To fix this, either:
#       (1) Use -fexternal-interpreter, or
#       (2) Build the program twice: once with -dynamic, and then
#           with -prof using -osuf to set a different object file suffix.
test('T8025', [extra_files(['A.hs', 'B.hs']), omit_ways(['profasm'])],
	      multimod_compile, ['A B', '-fno-code -v0'])