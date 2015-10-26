# --make -o without Main should be an error, not a warning.
test('T10895', normal, multimod_compile_fail, ['T10895.hs', '-v0 -o dummy'])
