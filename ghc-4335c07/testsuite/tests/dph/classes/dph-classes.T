
test('dph-classes-vseg-fast',
     [normal, reqlib('dph-lifted-vseg'), reqlib('dph-prim-par'),
      only_ways(['normal', 'threaded1', 'threaded2'])],
     multimod_compile_and_run,
     ['Main', '-O0 -fno-enable-rewrite-rules -package dph-lifted-vseg'])
