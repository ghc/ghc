test('T12010',
     [
        extra_files(['cbits/']),
        only_ways(['threaded1']),
        extra_ways(['threaded1']),
        cmd_prefix('WAY_FLAGS="' + ' '.join(config.way_flags['threaded1']) + '"')],
     run_command,
     ['$MAKE -s --no-print-directory T12010'])
