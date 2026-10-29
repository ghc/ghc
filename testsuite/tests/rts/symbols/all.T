test('rts-check-symbols', [
    unless(opsys('linux') and have_dynamic() and have_readelf() and have_ldd(), skip),
    windows_skip,
    # extra ways are important to test over different RTS DSOs
    extra_ways([
        'threaded1',    # threaded + debug
        'threaded2',    # threaded
        'sanity'        # debug
    ]),
    extra_files([
        'dumpDeclaredSymbols.c',
        'Main.hs',
        'rts-check-symbols.sh'
    ])],
    makefile_test,
    []
)
