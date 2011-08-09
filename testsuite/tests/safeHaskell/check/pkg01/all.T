# Just do the normal way, SafeHaskell is all in the frontend
def f( opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = ''

test('safePkg01',
     [normal, alone],
     run_command, 
     ['$MAKE -s --no-print-directory safePkg01 PROF=' + prof])

test('ImpSafe01', normal, compile_fail, ['-distrust base'])

test('ImpSafeOnly01', [normal, alone], compile, ['-package-conf local.db -trust base'])

test('ImpSafeOnly02', [normal, alone], compile, ['-package-conf local.db -trust base -trust safePkg01'])

test('ImpSafeOnly03', [normal, alone], compile_fail, ['-package-conf local.db -trust base'])

