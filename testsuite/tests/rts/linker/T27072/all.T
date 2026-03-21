test('T27072',
     [req_rts_linker,
      js_skip,
      extra_files(['Lib.c', 'main.c'])],
     makefile_test,
     ['clean_build_and_run'])
