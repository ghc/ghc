test('rts-check-symbols', [
    unless(have_dynamic(), skip),
    extra_files(['rts-check-symbols.sh', 'dumpDeclaredSymbols.c', 'Main.hs'])],
    makefile_test,
    []
)
