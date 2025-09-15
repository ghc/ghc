# This tests that recompilation logic works if you import a module with the same
# name
test('multipleHomeUnits_self-import', [extra_files([ 'p1/', 'p2/', 'unitP1', 'unitP2'])], makefile_test, [])

