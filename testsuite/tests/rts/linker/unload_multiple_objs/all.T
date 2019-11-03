test('linker_unload_multiple_objs',
     [extra_files(['../LinkerUnload.hs', 'A.hs', 'B.hs', 'C.hs', 'D.hs',]),
      unless(config.have_RTS_linker, skip)],
     run_command, ['$MAKE -s --no-print-directory linker_unload_multiple_objs'])
