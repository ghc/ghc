# safeLanguage tests are all about making sure the correct
# unsafe language extensions are disabled or restricted
# in the -XSafe language. Basically testing that -XSafe
# works correctly (including testing safe imports a little).

# Just do the normal way, SafeHaskell is all in the frontend
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('SafeLang01', normal, compile, [''])
test('SafeLang02', normal, compile, [''])
test('SafeLang03', normal, compile, [''])
test('SafeLang04', normal, compile_and_run, [''])
test('SafeLang05', normal, compile_and_run, [''])

# SafeLang06 was a test involving GeneralizedNewtypeDeriving, but the code
# fails to compile with roles; thus the test is no longer valid and has
# been removed

test('SafeLang07', normal, compile_fail, [''])
test('SafeLang08', normal, compile_fail, [''])
test('SafeLang09', [exit_code(1)], compile_and_run, [''])
test('SafeLang10', [], multimod_compile_fail, ['SafeLang10', ''])
test('SafeLang11', [req_interp], multimod_compile_and_run,
     ['SafeLang11', config.ghc_th_way_flags])
test('SafeLang12', normal, multimod_compile_fail, ['SafeLang12', ''])
test('SafeLang15', [exit_code(1)], multimod_compile_and_run,
     ['SafeLang15', '-XSafe'])
test('SafeLang16', normal, compile, [''])
test('SafeLang17', [], multimod_compile_fail, ['SafeLang17', ''])

test('SafeLang18', normal, compile, [''])

# Test building a package, that trust values are set correctly
# and can be changed correctly
#test('SafeRecomp01', normal, makefile_test, [])

