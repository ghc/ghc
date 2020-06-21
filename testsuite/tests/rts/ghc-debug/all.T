test('pause_and_unpause',
     [ extra_files(['pause_and_unpause_thread.c','pause_and_unpause_thread.h']),
      ignore_stdout,
      ignore_stderr
     ],
     multi_compile_and_run, ['pause_and_unpause', [('pause_and_unpause_thread.c','-optc=-g3 -optc=-O0 -opta=-g')], '-threaded -debug -O0 -g'])
