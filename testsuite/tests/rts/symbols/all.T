test('rts-check-symbols', [
    unless(have_dynamic(), skip),
    windows_skip,
    extra_files(['rts-check-symbols.sh', 'dumpDeclaredSymbols.c', 'Main.hs'])],
    makefile_test,
    []
)
