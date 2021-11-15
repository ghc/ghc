# Test for #3589, recompiling when #included files change

test('recomp011',
     [extra_files(['Main.hs']),
      when(opsys("OSJavaScript"), skip),
      when(arch('powerpc64') or arch('powerpc64le'), expect_broken(11260))],
     makefile_test, [])
