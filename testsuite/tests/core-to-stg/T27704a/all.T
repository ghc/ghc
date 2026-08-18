test('T27704a',
     [extra_files(['Main.hs', 'Mid.hs', 'Callee.hs', 'Callee.hs-boot'])],
     multimod_compile_and_run,
     ['Main', '-O -fspec-eval-dictfun'])
