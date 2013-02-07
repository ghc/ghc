# flags tests are about checking that various ghc option flags
# and some language extension flags still work. This used to
# test that safe compilation was working but now safe compilation
# has been dropped.

# Just do the normal way, SafeHaskell is all in the frontend
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('SafeFlags01', normal, compile, [''])
test('SafeFlags02', normal, compile, [''])
# no longer support a SafeImport flag, so expect fail for them.
test('SafeFlags03', normal, compile_fail, [''])

# no longer support a SafeLanguage flag, so expect fail for them.
test('SafeFlags04', normal, compile_fail, [''])

test('SafeFlags05', normal, compile_fail, [''])
test('SafeFlags06', normal, compile_fail, [''])

# no longer support a SafeLanguage flag, so expect fail for them.
test('SafeFlags07', normal, compile_fail, [''])

# no longer support a SafeLanguage flag, so expect fail for them.
test('SafeFlags08', normal, compile_fail, [''])
test('SafeFlags09', normal, compile_fail, [''])

test('SafeFlags10', normal, compile_fail, [''])
test('SafeFlags11', normal, compile, [''])

test('SafeFlags12', normal, compile, [''])
test('SafeFlags13', normal, compile_fail, [''])
test('SafeFlags14', normal, compile_fail, [''])

test('SafeFlags15', normal, compile, [''])
test('SafeFlags16', normal, compile, [''])
test('SafeFlags17', normal, compile_fail, ['-distrust base'])
test('SafeFlags18', normal, compile_fail, ['-trust base -Werror'])
test('SafeFlags19', normal, compile_fail, [''])
test('SafeFlags20', normal, compile, ['-trust base'])

# test -Wunsafe flag
test('SafeFlags21', normal, compile, [''])
test('SafeFlags22', normal, compile, [''])
test('SafeFlags23', normal, compile_fail, [''])

# test -Wsafe flag
test('SafeFlags24', normal, compile, [''])
test('SafeFlags25', normal, compile, [''])
test('SafeFlags26', normal, compile_fail, [''])

# test -fno-safe-infer
test('SafeFlags27', normal, compile, [''])

# test package flags don't work
test('SafeFlags28', normal, compile_fail, [''])
test('SafeFlags29', normal, compile_fail, [''])

# test certain flags are still allowed under -XSafe
test('Flags01', normal, compile, ['-XSafe'])
test('Flags02', normal, compile, ['-XSafe'])

