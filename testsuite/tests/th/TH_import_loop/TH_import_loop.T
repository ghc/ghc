
setTestOpts(when(compiler_profiled(), skip))

test('TH_import_loop',
     [extra_clean(['ModuleA.o-boot', 'ModuleA.hi-boot',
                   'ModuleC.o', 'ModuleC.hi']),
      expect_broken(1012)],
     multimod_compile_and_run,
     ['Main', '-v0'])

