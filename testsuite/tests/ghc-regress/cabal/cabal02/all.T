# cabal-setup isn't in the GHC tree any more; this test should probably
# be moved to the cabal-setup package.
test('cabal02', compose(skip, compose(compose(ignore_output, skip_if_fast), expect_broken_if_platform(1196, 'i386-unknown-mingw32'))), run_command, ['$MAKE cabal02'])
