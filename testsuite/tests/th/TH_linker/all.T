test('path_with_commas',
     [extra_files(['test.pkg', 'Main.hs', 'Dummy.hs']),
      ignore_stderr,
      unless(have_dynamic(),skip)],
     makefile_test, ['path_with_commas'])
