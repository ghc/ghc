test('ghcilink001',
     [when(ghci_dynamic(), expect_fail), # dynamic ghci can't load '.a's
      skip_if_no_ghci,
      extra_clean(['dir001/*','dir001'])],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink001'])

test('ghcilink002',
     [skip_if_no_ghci, extra_clean(['dir002/*','dir002'])],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink002'])

test('ghcilink003',
     [
       # still cannot load libstdc++ on Windows.  See also #4468.
       when(opsys('mingw32'), expect_broken(5289)),
       skip_if_no_ghci,
       extra_clean(['dir003/*','dir003'])
     ],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink003'])

test('ghcilink004',
     [when(ghci_dynamic(), expect_fail), # dynamic ghci can't load '.a's
      skip_if_no_ghci,
      extra_clean(['dir004/*','dir004'])],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink004'])

test('ghcilink005',
     [skip_if_no_ghci, extra_clean(['dir005/*','dir005'])],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink005'])

test('ghcilink006',
     [
       # still cannot load libstdc++ on Windows.  See also #4468.
       when(opsys('mingw32'), expect_broken(5289)),
       skip_if_no_ghci,
       extra_clean(['dir006/*','dir006'])
     ],
     run_command,
     ['$MAKE -s --no-print-directory ghcilink006'])
