test('ghcilink001',
     [when(ghci_dynamic(), expect_fail), # dynamic ghci can't load '.a's
      unless(doing_ghci, skip),
      extra_clean(['dir001/*','dir001'])],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink001'])

test('ghcilink002',
     [unless(doing_ghci, skip), extra_clean(['dir002/*','dir002'])],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink002'])

test('ghcilink003',
     [
       unless(doing_ghci, skip),
       extra_clean(['dir003/*','dir003'])
     ],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink003'])

test('ghcilink004',
     [unless(doing_ghci, skip),
      extra_clean(['dir004/local.package.conf/*', 'dir004/*', 'dir004'])
     ],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink004'])

test('ghcilink005',
     [unless(doing_ghci, skip),
      extra_clean(['dir005/ghcilink005.package.conf/*', 'dir005/*','dir005'])
     ],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink005'])

test('ghcilink006',
     [
       unless(doing_ghci, skip),
       extra_clean(['dir006/ghcilink006.package.conf/*', 'dir006/*','dir006'])
     ],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink006'])

test('T3333',
     [extra_clean(['T3333.o']),
     unless(doing_ghci, skip),
     unless(ghci_dynamic(), expect_broken(3333))],
     run_command,
     ['$MAKE -s --no-print-directory T3333'])

