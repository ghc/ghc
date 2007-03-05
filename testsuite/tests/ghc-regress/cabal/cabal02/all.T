test('cabal02', compose(compose(ignore_output, skip_if_fast), expect_broken_if_platform(1196, 'i386-unknown-mingw32')), run_command, ['$MAKE cabal02'])
