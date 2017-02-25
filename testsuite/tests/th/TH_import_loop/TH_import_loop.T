
test('TH_import_loop', [extra_files(['Main.hs', 'ModuleA.hs', 'ModuleA.hs-boot', 'ModuleB.hs', 'ModuleC.hs']),
                        expect_broken(1012)], multimod_compile_and_run,
     ['Main', '-v0'])
