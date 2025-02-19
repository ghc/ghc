test('T12010',
     [extra_files(['cbits/']),
      only_ways(['threaded1']),
      extra_ways(['threaded1']),
      when(wordsize(32), fragile(16572)),
      js_broken(22374),
      req_target_debug_rts,
      cmd_prefix('WAY_FLAGS="' + ' '.join(config.way_flags['threaded1']) + '"')],
     makefile_test, [])
