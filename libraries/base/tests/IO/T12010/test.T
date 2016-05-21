test('T12010',
     [
        extra_clean(['cbits/initWinSock.o', 'T12010.hi', 'T12010.hs']),
        only_ways(['threaded1']),
        extra_ways(['threaded1']),
        exit_code(0),
        ignore_output,
        cmd_prefix('WAY_FLAGS="' + ' '.join(config.way_flags('T12010')['threaded1']) + '"')],
     run_command,
     ['$MAKE -s --no-print-directory T12010'])
