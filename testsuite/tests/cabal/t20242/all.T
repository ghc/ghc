if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('T20242',
     [extra_files(['Setup.hs', 'BootNoHeader.cabal','Foo.hs', 'Foo.hs-boot', 'Main.hs']), js_broken(22349)],
     run_command,
     ['$MAKE -s --no-print-directory T20242 ' + cleanup])
