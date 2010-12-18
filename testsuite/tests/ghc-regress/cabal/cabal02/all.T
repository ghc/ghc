# cabal-setup isn't in the GHC tree any more; this test should probably
# be moved to the cabal-setup package.
test('cabal02',
     [skip,
      skip_if_fast,
      if_platform('i386-unknown-mingw32', expect_broken(1196))],
     run_command,
     ['$MAKE -s --no-print-directory cabal02'])
