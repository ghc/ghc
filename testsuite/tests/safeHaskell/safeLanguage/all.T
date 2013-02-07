# safeLanguage tests are all about making sure the correct
# unsafe language extensions are disabled or restricted
# in the -XSafe language. Basically testing that -XSafe
# works correctly (incluidng testing safe imports a little).

# Just do the normal way, SafeHaskell is all in the frontend
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('SafeLang01', normal, compile, [''])
test('SafeLang02', normal, compile, [''])
test('SafeLang03', normal, compile, [''])
test('SafeLang04', normal, compile_and_run, [''])
test('SafeLang05', normal, compile_and_run, [''])
test('SafeLang06',
     extra_clean(['SafeLang06_A.o', 'SafeLang06_A.hi']),
     compile_and_run,
     [''])
test('SafeLang07', normal, compile_fail, [''])
test('SafeLang08', normal, compile_fail, [''])
test('SafeLang09',
     [exit_code(1),
      extra_clean(['SafeLang09_A.o', 'SafeLang09_A.hi',
                   'SafeLang09_B.o', 'SafeLang09_B.hi'])],
     compile_and_run,
     [''])
test('SafeLang10',
     extra_clean(['SafeLang10_A.o', 'SafeLang10_A.hi',
                  'SafeLang10_B.o', 'SafeLang10_B.hi']),
     multimod_compile_fail,
     ['SafeLang10', ''])
test('SafeLang11',
     [req_interp,
      extra_clean(['SafeLang11_A.o', 'SafeLang11_A.hi',
                   'SafeLang11_B.o', 'SafeLang11_B.hi'])],
     multimod_compile_and_run,
     ['SafeLang11', ''])
test('SafeLang12', normal, multimod_compile_fail, ['SafeLang12', ''])
test('SafeLang13',
     extra_clean(['SafeLang13_A.o', 'SafeLang13_A.hi']),
     multimod_compile_and_run,
     ['SafeLang13', ''])
test('SafeLang14',
     extra_clean(['SafeLang14_A.o', 'SafeLang14_A.hi']),
     multimod_compile_fail,
     ['SafeLang14', '-XSafe'])
test('SafeLang15',
     [exit_code(1),
      extra_clean(['SafeLang15_A.o', 'SafeLang15_A.hi'])],
     multimod_compile_and_run,
     ['SafeLang15', '-XSafe'])
test('SafeLang16', normal, compile, [''])

# Test building a package, that trust values are set correctly
# and can be changed correctly
#test('SafeRecomp01',
#		clean_cmd('$MAKE -s --no-print-directory cleanSafeRecomp01'),
#      run_command, ['$MAKE -s --no-print-directory safeRecomp01'])

