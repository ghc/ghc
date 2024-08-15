test('T24384',
     [extra_files([
         'A.hs',
         'B.hs',
         'C.hs',
         'C.hs-boot',
         'D.hs',
         'E.hs',
         'F.hs',
         'F.hs-boot',
         'preproc.sh',
         'setup-dep.sh',
         ]),
      # We're using a bash script as a preprocessor, which appears not to work on windows.
      when(opsys('mingw32'), skip),
      ],
     makefile_test,
     [])
