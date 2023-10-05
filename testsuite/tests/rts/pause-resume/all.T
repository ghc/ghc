test('pause_resume_via_safe_ffi',
     [ only_ways(['threaded1', 'threaded2'])
     , req_c
     , extra_files(['pause_resume.c','pause_resume.h'])
     ],
     multi_compile_and_run, ['pause_resume_via_safe_ffi', [('pause_resume.c','')], ''])
test('pause_resume_via_pthread',
     [ only_ways(['threaded1', 'threaded2'])
     , req_c
     , extra_files(['pause_resume.c','pause_resume.h'])
     ],
     multi_compile_and_run, ['pause_resume_via_pthread', [('pause_resume.c','')], ''])
test('pause_resume_via_safe_ffi_concurrent',
     [ only_ways(['threaded1', 'threaded2'])
     , req_c
     , extra_files(['pause_resume.c','pause_resume.h'])
     ],
     multi_compile_and_run, ['pause_resume_via_safe_ffi_concurrent', [('pause_resume.c','')], ''])
test('pause_and_use_rts_api',
     [ only_ways(['threaded1', 'threaded2'])
     , req_c
     , extra_files(['pause_resume.c','pause_resume.h'])
     ],
     multi_compile_and_run, ['pause_and_use_rts_api', [('pause_resume.c','')], ''])
test('list_threads_and_misc_roots',
     [ only_ways(['threaded1', 'threaded2'])
     , req_c
     , extra_files(['list_threads_and_misc_roots_c.c','list_threads_and_misc_roots_c.h'])
     ],
     multi_compile_and_run, ['list_threads_and_misc_roots', [('list_threads_and_misc_roots_c.c','')], ''])
