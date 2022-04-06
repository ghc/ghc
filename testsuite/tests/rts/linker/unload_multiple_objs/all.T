test('linker_unload_multiple_objs',
     [extra_files(['../LinkerUnload.hs', 'A.hs', 'B.hs', 'C.hs', 'D.hs',]),
      when(opsys('mingw32'), expect_broken(21354)),
      req_rts_linker],
     run_command, ['$MAKE -s --no-print-directory linker_unload_multiple_objs'])
