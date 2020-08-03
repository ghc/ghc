test('specializable',
     [extra_files(['SpecNoinlineLib.hs', 'User.hs']), ignore_stdout],
     makefile_test, [])

