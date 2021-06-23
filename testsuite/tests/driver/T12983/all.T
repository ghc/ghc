# Test for #12983, a recompilation bug with Template Haskell
test('T12983', [req_th, extra_files(['build.sh', 'src'])], run_command, ['./build.sh'])

