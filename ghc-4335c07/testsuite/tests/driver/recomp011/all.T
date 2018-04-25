# Test for #3589, recompiling when #included files change

test('recomp011',
     [extra_files(['Main.hs']),
      when(arch('powerpc64') or arch('powerpc64le'), expect_broken(11260))],
     run_command, ['$MAKE -s --no-print-directory recomp011'])
