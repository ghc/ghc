test('rts-check-symbols', [
    unless(have_dynamic() and have_readelf() and have_ldd(), skip),
    windows_skip,
    extra_files([
        'dumpDeclaredSymbols.c',
        'Main.hs',
        'rts-check-symbols.ignore',
        'rts-check-symbols.sh'
    ])],
    makefile_test,
    []
)
