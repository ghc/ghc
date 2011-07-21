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
test('SafeLang10', normal, compile_fail, ['--make -trust base'])
test('SafeLang11', req_interp, compile_and_run, ['--make -trust base'])
test('SafeLang12', normal, compile_fail, ['--make -trust base'])
test('SafeLang13', normal, compile_and_run, ['--make -trust base'])
test('SafeLang14', normal, compile_fail, ['--make -trust base -XSafe'])
test('SafeLang15', exit_code(1), compile_and_run, ['--make -trust base -XSafe'])

