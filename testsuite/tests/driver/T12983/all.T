# Test for #12983, a recompilation bug with Template Haskell
test('T12983', [req_th, unless(have_dynamic(), skip), extra_files(['build.sh', 'src'])], makefile_test, [])

