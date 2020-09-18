test('rts_pause_and_resume',
     [ extra_files(['rts_pause_and_resume_c.c','rts_pause_and_resume_c.h']),
      ignore_stdout,
      ignore_stderr
     ],
     multi_compile_and_run, ['rts_pause_and_resume', [('rts_pause_and_resume_c.c','')], '-threaded '])