
# This test is in its own subdirectory as it needs to use the global
# filename "Main.imports"

test('T7476', extra_clean(['Main.imports']), run_command,
     ['$MAKE -s --no-print-directory T7476'])
