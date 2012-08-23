# Just do the normal way, SafeHaskell is all in the frontend
def f( opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = ''

# Test building a package, that trust values are set correctly
# and can be changed correctly
test('safePkg01',
     [# TODO: The other tests in here depend on this one not being cleaned
      # clean_cmd('$MAKE -s --no-print-directory cleanSafePkg01'),
      alone],
     run_command, 
     ['$MAKE -s --no-print-directory safePkg01 PROF=' + prof])

# Fail since we enable package trust
test('ImpSafe01', normal, compile_fail, ['-fpackage-trust -distrust base'])

# Succeed since we don't enable package trust
test('ImpSafe02', normal, compile, ['-distrust base'])

test('ImpSafeOnly01', [normal, alone], compile, ['-fpackage-trust -package-db local.db -trust base'])

test('ImpSafeOnly02', [normal, alone], compile, ['-fpackage-trust -package-db local.db -trust base -trust safePkg01'])

# Fail since we enable package trust (and still need safePkg01 trusted)
test('ImpSafeOnly03', [normal, alone], compile_fail, ['-fpackage-trust -package-db local.db -trust base'])

# Succeed since we don't enable package trust
test('ImpSafeOnly04', [normal, alone], compile, ['-package-db local.db -trust base'])

# fail due to missing trust of safePkg01, next test succeeds.
test('ImpSafeOnly05', [normal, alone], compile_fail, ['-fpackage-trust -package-db local.db -trust base'])
test('ImpSafeOnly06', [normal, alone], compile, ['-fpackage-trust -package-db local.db -trust base -trust safePkg01'])

# fail due to missing trust
test('ImpSafeOnly07', [normal, alone], compile_fail, ['-fpackage-trust -package-db local.db -trust safePkg01'])
test('ImpSafeOnly08', [normal, alone], compile_fail, ['-fpackage-trust -package-db local.db -trust safePkg01'])
test('ImpSafeOnly09', [normal, alone], compile_fail, ['-fpackage-trust -package-db local.db -trust safePkg01'])

# finally succeed
test('ImpSafeOnly10', [normal, alone], compile, ['-fpackage-trust -package-db local.db -trust safePkg01 -trust base -trust bytestring'])

