test('T18382',
     [unless(opsys('mingw32'), skip),
      extra_run_opts('+RTS --io-manager=native -RTS'),
      expect_broken(18382),
     ],
     compile_and_run,
     [''])
