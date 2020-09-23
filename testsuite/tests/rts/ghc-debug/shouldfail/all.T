
test('unsafe_rts_pause', [only_ways(['threaded1']), exit_code(1)], compile_and_run, [''])
test('rts_lock_when_paused_deadlock',
    [ only_ways(['threaded1', 'threaded2']),
      extra_ways(['threaded1', 'threaded2']),
      exit_code(1),
      extra_files(['deadlock.c','deadlock.h'])
    ],
    multi_compile_and_run, ['rts_lock_when_paused_deadlock', [('deadlock.c','')], ''])
test('rts_pause_when_locked_deadlock',
    [ only_ways(['threaded1', 'threaded2']),
      extra_ways(['threaded1', 'threaded2']),
      exit_code(1),
      extra_files(['deadlock.c','deadlock.h'])
    ],
    multi_compile_and_run, ['rts_pause_when_locked_deadlock', [('deadlock.c','')], ''])