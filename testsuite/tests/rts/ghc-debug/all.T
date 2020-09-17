test('rts_pause_and_unpause',
     [ extra_files(['rts_pause_and_unpause_c.c','rts_pause_and_unpause_c.h']),
      ignore_stdout,
      ignore_stderr
     ],
     multi_compile_and_run, ['rts_pause_and_unpause', [('rts_pause_and_unpause_c.c','')], '-threaded '])