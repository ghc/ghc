test('linker_unload_multiple_objs',
     [extra_files(['../LinkerUnload.hs', 'A.hs', 'B.hs', 'C.hs', 'D.hs',]),
      when(arch('powerpc64') or arch('powerpc64le'), expect_broken(11259))],
     run_command, ['$MAKE -s --no-print-directory linker_unload_multiple_objs'])
