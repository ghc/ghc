def check_nothing(actual_file, normaliser):
  actual_raw = read_no_crs(actual_file)
  return ("Nothing" in actual_raw)


test('SI01', normal, multimod_compile, ['SI01', '-v0'])
test('SI02', normal, compile, [''])
test('SI03', [extra_files(["SI01A.hs"])], multimod_compile_fail, ['SI03', '-v0'])
test('SI04', [extra_files(["SI01A.hs"])], multimod_compile, ['SI04', '-v0'])
test('SI05', [extra_files(["SI01A.hs"])], multimod_compile_fail, ['SI05', '-v0'])
test('SI06', [extra_files(["SI01A.hs"])], multimod_compile, ['SI06', '-v0'])
test('SI07', [extra_files(["SI05A.hs"])], multimod_compile, ['SI07', '-fwrite-interface -fno-code'])
# Instance tests
test('SI08', [extra_files(["ClassA.hs", "InstanceA.hs"])], multimod_compile_fail, ['SI08', '-v0'])
test('SI09', [extra_files(["ClassA.hs", "InstanceA.hs"])], multimod_compile, ['SI09', '-v0'])
test('SI10', [extra_files(["ClassA.hs", "InstanceA.hs"])], multimod_compile, ['SI10', '-v0'])

