test('pause_resume_via_safe_ffi',
     [ extra_ways(['threaded1', 'threaded2'])
     , only_ways(['threaded1', 'threaded2'])
     , extra_files(['pause_resume.c','pause_resume.h'])
     ],
     multi_compile_and_run, ['pause_resume_via_safe_ffi', [('pause_resume.c','')], ''])
test('pause_resume_via_pthread',
     [ extra_ways(['threaded1', 'threaded2'])
     , only_ways(['threaded1', 'threaded2'])
     , extra_files(['pause_resume.c','pause_resume.h'])
     ],
     multi_compile_and_run, ['pause_resume_via_pthread', [('pause_resume.c','')], ''])
test('pause_resume_via_safe_ffi_concurrent',
     [ extra_ways(['threaded1', 'threaded2'])
     , only_ways(['threaded1', 'threaded2'])
     , extra_files(['pause_resume.c','pause_resume.h'])
     ],
     multi_compile_and_run, ['pause_resume_via_safe_ffi_concurrent', [('pause_resume.c','')], ''])
test('pause_and_use_rts_api',
     [ extra_ways(['threaded1', 'threaded2'])
     , only_ways(['threaded1', 'threaded2'])
     , extra_files(['pause_resume.c','pause_resume.h'])
     ],
     multi_compile_and_run, ['pause_and_use_rts_api', [('pause_resume.c','')], ''])
