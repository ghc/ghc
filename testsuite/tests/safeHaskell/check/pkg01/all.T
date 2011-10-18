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

test('ImpSafeOnly01', [normal, alone], compile, ['-package-conf local.db -trust base'])

test('ImpSafeOnly02', [normal, alone], compile, ['-package-conf local.db -trust base -trust safePkg01'])

# Fail since we enable package trust
test('ImpSafeOnly03', [normal, alone], compile_fail, ['-fpackage-trust -package-conf local.db -trust base'])

# Succeed since we don't enable package trust
test('ImpSafeOnly04', [normal, alone], compile, ['-package-conf local.db -trust base'])

