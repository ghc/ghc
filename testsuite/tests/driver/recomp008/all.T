# Test for #4469, a recompilation bug related to instances

test('recomp008', [when(fast(), skip), normalise_slashes], run_command,
     ['$MAKE -s --no-print-directory recomp008'])
