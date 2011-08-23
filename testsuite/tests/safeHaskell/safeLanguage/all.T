# Just do the normal way, SafeHaskell is all in the frontend
def f( opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('SafeLang01', normal, compile, ['-trust base'])
test('SafeLang02', normal, compile, ['-trust base'])
test('SafeLang03', normal, compile, ['-trust base'])
test('SafeLang04', normal, compile_and_run, [''])
test('SafeLang05', normal, compile_and_run, ['-trust base'])
test('SafeLang06', extra_clean(['SafeLang06_A.hi', 'SafeLang06_A.o']), compile_and_run, [''])
test('SafeLang07', normal, compile_fail, [''])
test('SafeLang08', normal, compile_fail, [''])
test('SafeLang09', compose(exit_code(1), extra_clean(['SafeLang09_A.hi', 'SafeLang09_A.o'])), compile_and_run, [''])
test('SafeLang10', extra_clean(['SafeLang10_A.hi', 'SafeLang10_A.o']), multimod_compile_fail, ['SafeLang10', '-trust base'])
test('SafeLang11', compose(req_interp, extra_clean(['SafeLang11_A.hi', 'SafeLang11_A.o', 'SafeLang11_B.hi', 'SafeLang11_B.o'])), multimod_compile_and_run, ['SafeLang11', '-trust base'])
test('SafeLang12', normal, multimod_compile_fail, ['SafeLang12', '-trust base'])
test('SafeLang13', extra_clean(['SafeLang13_A.hi', 'SafeLang13_A.o']), multimod_compile_and_run, ['SafeLang13', '-trust base'])
test('SafeLang14', normal, multimod_compile_fail, ['SafeLang14', '-trust base -XSafe'])
test('SafeLang15', compose(exit_code(1), extra_clean(['SafeLang15_A.hi', 'SafeLang15_A.o'])), multimod_compile_and_run, ['SafeLang15', '-trust base -XSafe'])

