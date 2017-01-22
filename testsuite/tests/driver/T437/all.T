# Test for #437, a recompilation bug with '-main-is'

test('T437', [], run_command, ['$MAKE -s --no-print-directory T437'])
