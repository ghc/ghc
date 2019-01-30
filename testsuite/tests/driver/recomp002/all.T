test('recomp002', [extra_files(['Q.hs', 'W.hs', 'W.hs-boot']),
                   when(fast(), skip)], run_command,
     ['$MAKE -s --no-print-directory recomp002'])
