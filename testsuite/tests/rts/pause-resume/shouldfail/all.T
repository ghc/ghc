
test('unsafe_rts_pause',
    [ only_ways(['threaded1', 'threaded2'])
    , exit_code(1)
    ], compile_and_run, [''])
test('rts_lock_when_paused',
    [ only_ways(['threaded1', 'threaded2'])
    , exit_code(1)
    , req_c
    , extra_files(['rts_pause_lock.c','rts_pause_lock.h'])
    ],
    multi_compile_and_run, ['rts_lock_when_paused', [('rts_pause_lock.c','')], ''])
test('rts_pause_when_locked',
    [ only_ways(['threaded1', 'threaded2'])
    , exit_code(1)
    , req_c
    , extra_files(['rts_pause_lock.c','rts_pause_lock.h'])
    ],
    multi_compile_and_run, ['rts_pause_when_locked', [('rts_pause_lock.c','')], ''])
test('rts_double_pause',
    [ only_ways(['threaded1', 'threaded2'])
    , exit_code(1)
    , req_c
    , extra_files(['rts_pause_lock.c','rts_pause_lock.h'])
    ],
    multi_compile_and_run, ['rts_double_pause', [('rts_pause_lock.c','')], ''])
