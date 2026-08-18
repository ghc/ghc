test('T27717',
     [extra_files(['Main.hs', 'Mid.hs', 'Ty.hs', 'Callee.hs', 'Callee.hs-boot'])],
     multimod_compile_and_run,
     ['Main', '-O -fspec-eval-dictfun'])
