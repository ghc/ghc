# Just do the normal way, SafeHaskell is all in the frontend
def f( opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('SafeLang01', normal, compile, ['-trust base'])
test('SafeLang02', normal, compile, ['-trust base'])
test('SafeLang03', normal, compile, ['-trust base'])
test('SafeLang04', normal, compile_and_run, [''])
test('SafeLang05', normal, compile_and_run, ['-trust base'])
test('SafeLang06', normal, compile_and_run, [''])
test('SafeLang07', normal, compile_fail, [''])
test('SafeLang08', normal, compile_fail, [''])
test('SafeLang09', exit_code(1), compile_and_run, [''])
test('SafeLang10', normal, multimod_compile_fail, ['SafeLang10', '-trust base'])
test('SafeLang11', req_interp, multimod_compile_and_run, ['SafeLang11', '-trust base'])
test('SafeLang12', normal, multimod_compile_fail, ['SafeLang12', '-trust base'])
test('SafeLang13', normal, multimod_compile_and_run, ['SafeLang13', '-trust base'])
test('SafeLang14', normal, multimod_compile_fail, ['SafeLang14', '-trust base -XSafe'])
test('SafeLang15', exit_code(1), multimod_compile_and_run, ['SafeLang15', '-trust base -XSafe'])

