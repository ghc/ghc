test('base01', [extra_files(['GHC']),
                normalise_slashes], run_command,
     ['$MAKE -s base01 --no-print-directory'])
