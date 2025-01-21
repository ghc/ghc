def check_nothing(actual_file, normaliser):
  actual_raw = read_no_crs(actual_file)
  return ("Nothing" in actual_raw)


test('SI01', normal, multimod_compile, ['SI01', '-v0'])
test('SI02', normal, multimod_compile, ['SI02', '-v0'])
test('SI03', [extra_files(["SI01A.hs"])], multimod_compile_fail, ['SI03', '-v0'])
test('SI04', [extra_files(["SI01A.hs"])], multimod_compile, ['SI04', '-v0'])
test('SI05', [extra_files(["SI01A.hs"])], multimod_compile_fail, ['SI05', '-v0'])
test('SI06', [extra_files(["SI01A.hs"])], multimod_compile, ['SI06', '-v0'])
test('SI07', [unless(ghc_dynamic(), skip), extra_files(["SI05A.hs"])], multimod_compile, ['SI07', '-fwrite-interface -fno-code'])
# Instance tests
test('SI08', [extra_files(["ClassA.hs", "InstanceA.hs"])], multimod_compile_fail, ['SI08', '-v0'])
test('SI09', [extra_files(["ClassA.hs", "InstanceA.hs"])], multimod_compile, ['SI09', '-v0'])
test('SI10', [extra_files(["ClassA.hs", "InstanceA.hs"])], multimod_compile, ['SI10', '-v0'])
# Instance tests with oneshot mode
test('SI08_oneshot', [extra_files(["ClassA.hs", "InstanceA.hs", "SI08.hs"])], makefile_test, [])
test('SI09_oneshot', [extra_files(["ClassA.hs", "InstanceA.hs", "SI09.hs"])], makefile_test, [])
test('SI10_oneshot', [extra_files(["ClassA.hs", "InstanceA.hs", "SI10.hs"])], makefile_test, [])
test('SI13', normal,  multimod_compile, ['SI13', '-v0'])
test('SI14', normal,  compile_fail, [''])
test('SI15', normal,  compile_fail, [''])
test('SI16', normal,  compile_fail, [''])
test('SI17', normal,  compile, [''])
test('SI18', normal,  compile_fail, [''])
test('SI19', extra_files(["SI19A.hs"]),  multimod_compile, ['SI19', '-v0'])
test('SI20', extra_files(["SI19A.hs"]),  multimod_compile_fail, ['SI20', '-v0'])
test('SI21', normal,  multimod_compile_fail, ['SI21', '-v0'])
test('SI22', normal,  multimod_compile_fail, ['SI22', '-v0'])
test('SI23', extra_files(["SI23A.hs"]),  multimod_compile, ['SI23', '-v0'])
test('SI24', normal, compile, [''])
test('SI25', extra_files(["SI25Helper.hs"]), multimod_compile_fail, ['SI25', '-v0'])
test('SI26', normal, compile, [''])
test('SI27', normal, compile_fail, [''])
test('SI28', normal, compile_fail, [''])
test('SI29', normal, compile_fail, [''])
test('SI30', [only_ways(['ghci']), extra_hc_opts("-XExplicitLevelImports")], ghci_script, ['SI30.script'])
test('SI31', [only_ways(['ghci']), extra_hc_opts("-XExplicitLevelImports -XTemplateHaskell")], ghci_script, ['SI31.script'])
test('SI32', [only_ways(['ghci']), extra_hc_opts("-XExplicitLevelImports -XTemplateHaskell")], ghci_script, ['SI32.script'])
test('SI33', [only_ways(['ghci']), extra_hc_opts("-XExplicitLevelImports -XTemplateHaskell")], ghci_script, ['SI33.script'])
test('SI34', [extra_files(["SI34M1.hs", "SI34M2.hs"])], multimod_compile, ['SI34', '-fno-code'])
test('SI35',
     [extra_run_opts(f'"{config.libdir}"'),
     extra_files(['SI35A.hs'])],
     compile_and_run,
     ['-package ghc'])
test('SI36', [extra_files(["SI36_A.hs", "SI36_B1.hs", "SI36_B2.hs", "SI36_B3.hs", "SI36_C1.hs", "SI36_C2.hs", "SI36_C3.hs"])], multimod_compile_fail, ['SI36', '-v0'])
