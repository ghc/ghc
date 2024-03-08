test('T24171',
     [req_rts_linker,
      req_profiling,
      extra_files(['Lib.hs', 'main.c']),
      fragile(24512)],
     makefile_test,
     ['clean_build_and_run'])
