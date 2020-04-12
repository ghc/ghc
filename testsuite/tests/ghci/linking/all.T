test('ghcilink001',
     [extra_files(['TestLink.hs', 'f.c']),
      when(ghc_dynamic(), expect_fail), # dynamic ghci can't load '.a's
      unless(doing_ghci, skip),
      extra_clean(['dir001/*','dir001'])],
     makefile_test, ['ghcilink001'])

test('ghcilink002', [extra_files(['TestLink.hs', 'f.c']),
                     when(unregisterised(), fragile(16085)),
                     unless(doing_ghci, skip)],
     makefile_test, ['ghcilink002'])

test('ghcilink003',
     [unless(doing_ghci, skip),
      # libstdc++ is named differently on FreeBSD
      when(opsys('freebsd'), expect_broken(17739))],
     makefile_test,
     ['ghcilink003'])

test('ghcilink004',
     [extra_files(['TestLink.hs', 'f.c']),
      unless(doing_ghci, skip),
      when(unregisterised(), fragile(16085)),
      req_rts_linker],
     makefile_test, ['ghcilink004'])

test('ghcilink005',
     [extra_files(['TestLink.hs', 'f.c']),
      when(unregisterised(), fragile(16085)),
      unless(doing_ghci, skip)],
     makefile_test, ['ghcilink005'])

test('ghcilink006',
     [unless(doing_ghci, skip),
      # libstdc++ is named differently on FreeBSD
      when(opsys('freebsd'), expect_broken(17739))],
     makefile_test,
     ['ghcilink006'])

test('T3333',
     [unless(doing_ghci, skip),
      unless(opsys('linux') or opsys('darwin') or ghc_dynamic(),
             expect_broken(3333))],
     makefile_test, ['T3333'])

test('T11531',
     [extra_files(['T11531.hs', 'T11531.c', 'T11531.h']),
      unless(doing_ghci, skip),
      unless(opsys('linux'), skip),
      fragile(11531)],
     makefile_test, ['T11531'])

test('T14708',
     [extra_files(['T14708.hs', 'add.c']),
      unless(doing_ghci, skip),
      unless(ghc_dynamic(), skip),
      extra_clean(['T14708scratch/*', 'T14708'])],
     makefile_test, ['T14708'])

test('T15729',
     [extra_files(['T15729.hs', 'T15729.c']),
      unless(doing_ghci, skip)],
     makefile_test, ['T15729'])
