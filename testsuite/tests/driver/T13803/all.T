test('T13803',
     [extra_files(['D.hs', 'E.hs-boot', 'E.hs', 'Y.hs', 'Y.hs-boot'])],
     run_command,
     ['$MAKE -s --no-print-directory T13803'])
