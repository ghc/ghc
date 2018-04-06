test('T13914',
     [extra_files(['main.hs'])],
     run_command,
     ['$MAKE -s --no-print-directory t13914'])
