
setTestOpts(if_compiler_profiled(skip))

test('ghci001', normal, ghci_script, ['ghci001.script'])
test('ghci002', normal, ghci_script, ['ghci002.script'])
test('ghci003', normal, ghci_script, ['ghci003.script'])
test('ghci004', normal, ghci_script, ['ghci004.script'])
test('ghci005', normal, ghci_script, ['ghci005.script'])
test('ghci006', normal, ghci_script, ['ghci006.script'])
test('ghci007', normal, ghci_script, ['ghci007.script'])
test('ghci008', normal, ghci_script, ['ghci008.script'])
test('ghci009', normal, ghci_script, ['ghci009.script'])

# Skip this test: deadlock can't be detected now, because we wait for
# signals to arrive if there are signal handlers installed, and GHCi
# has a ^C handler installed.
test('ghci010', skip, ghci_script, ['ghci010.script'])

test('ghci011', normal, ghci_script, ['ghci011.script'])
test('ghci012', normal, ghci_script, ['ghci012.script'])
test('ghci013', normal, ghci_script, ['ghci013.script'])
test('ghci014', reqlib('QuickCheck'), ghci_script, ['ghci014.script'])

# Ignore output from ghci015, the output is non-deterministic
# (has occasional "thread blocked indefinitely" exceptions), and this
# is part of the test.
test('ghci015', ignore_output, ghci_script, ['ghci015.script'])

test('ghci016', expect_broken(552), ghci_script, ['ghci016.script'])
test('ghci017', extra_run_opts('-hide-package haskell98'), ghci_script, ['ghci017.script'])
test('ghci018', expect_broken(1201), ghci_script, ['ghci018.script'])
test('ghci019', normal, ghci_script, ['ghci019.script'])
test('ghci020', normal, ghci_script, ['ghci020.script'])
test('ghci021', normal, ghci_script, ['ghci021.script'])
test('ghci022', normal, ghci_script, ['ghci022.script'])

test('ghci023', normal, ghci_script, ['ghci023.script'])
test('ghci024',
     [skip_if_fast,
      if_platform("powerpc-apple-darwin", expect_broken(1845))],
     run_command,
     ['$MAKE -s --no-print-directory ghci024'])
test('ghci025', normal, ghci_script, ['ghci025.script'])
test('ghci026', normal, ghci_script, ['ghci026.script'])

test('ghci027', normal, ghci_script, ['ghci027.script'])
test('ghci028', normal, ghci_script, ['ghci028.script'])
test('ghci029', normal, ghci_script, ['ghci029.script'])
test('ghci030', normal, ghci_script, ['ghci030.script'])
test('ghci031', normal, ghci_script, ['ghci031.script'])
test('ghci032', normal, ghci_script, ['ghci032.script'])
test('ghci033', normal, ghci_script, ['ghci033.script'])
test('ghci034', normal, ghci_script, ['ghci034.script'])
test('ghci035', normal, ghci_script, ['ghci035.script'])
test('ghci036', normal, ghci_script, ['ghci036.script'])
test('2452', normal, ghci_script, ['2452.script'])
test('T2766', normal, ghci_script, ['T2766.script'])

test('1914',
     extra_clean(['T1914A.hs', 'T1914B.hs']),
     ghci_script,
     ['1914.script'])

test('2976', normal, ghci_script, ['2976.script'])
test('2816', normal, ghci_script, ['2816.script'])
test('T789', normal, ghci_script, ['T789.script'])
test('T3263', normal, ghci_script, ['T3263.script'])
test('4051', normal, ghci_script, ['4051.script'])
test('T4087', normal, ghci_script, ['T4087.script'])
test('T4015', normal, ghci_script, ['T4015.script'])
test('T4127', normal, ghci_script, ['T4127.script'])
test('T4127a', normal, ghci_script, ['T4127a.script'])
