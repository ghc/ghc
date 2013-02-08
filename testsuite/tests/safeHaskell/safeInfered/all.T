# safeInfered tests are for testing that the Safe Haskell
# mode safe inference works correctly.

# Just do the normal way, SafeHaskell is all in the frontend
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

# Tests that should compile fine as they should be infered safe
test('SafeInfered01',
     [ extra_clean(['SafeInfered01_A.hi', 'SafeInfered01_A.o']) ],
     multimod_compile, ['SafeInfered01', ''])
test('SafeInfered02',
     [ extra_clean(['SafeInfered02_A.hi', 'SafeInfered02_A.o']) ],
     multimod_compile, ['SafeInfered02', ''])
test('SafeInfered03',
     [ extra_clean(['SafeInfered03_A.hi', 'SafeInfered03_A.o']) ],
      multimod_compile, ['SafeInfered03', ''])
test('SafeInfered04',
     [ extra_clean(['SafeInfered04_A.hi', 'SafeInfered04_A.o']) ],
     multimod_compile, ['SafeInfered04', ''])

# Tests that should fail to compile as they should be infered unsafe
test('UnsafeInfered01',
     [ extra_clean(['UnsafeInfered01_A.hi', 'UnsafeInfered01_A.o']) ],
     multimod_compile_fail, ['UnsafeInfered01', ''])
test('UnsafeInfered02',
     [ extra_clean(['UnsafeInfered02_A.hi', 'UnsafeInfered02_A.o']) ],
     multimod_compile_fail, ['UnsafeInfered02', ''])
test('UnsafeInfered03',
     [ extra_clean(['UnsafeInfered03_A.hi', 'UnsafeInfered03_A.o']) ],
     multimod_compile_fail, ['UnsafeInfered03', ''])
test('UnsafeInfered05',
     [ extra_clean(['UnsafeInfered05_A.hi', 'UnsafeInfered05_A.o']) ],
     multimod_compile_fail, ['UnsafeInfered05', ''])
test('UnsafeInfered06',
     [ extra_clean(['UnsafeInfered06_A.hi', 'UnsafeInfered06_A.o']) ],
     multimod_compile_fail, ['UnsafeInfered06', ''])
test('UnsafeInfered07',
     [ extra_clean(['UnsafeInfered07_A.hi', 'UnsafeInfered07_A.o']) ],
     multimod_compile_fail, ['UnsafeInfered07', ''])
test('UnsafeInfered08',
     [ extra_clean(['UnsafeInfered08_A.hi', 'UnsafeInfered08_A.o']) ],
     multimod_compile_fail, ['UnsafeInfered08', ''])
test('UnsafeInfered09',
     [ extra_clean(['UnsafeInfered09_A.hi', 'UnsafeInfered09_A.o',
                    'UnsafeInfered09_B.hi', 'UnsafeInfered09_B.o']) ],
     multimod_compile_fail, ['UnsafeInfered09', ''])

# Test that should fail as we disable safe inference
test('UnsafeInfered10',
     [ extra_clean(['UnsafeInfered10_A.hi', 'UnsafeInfered10_A.o']) ],
     multimod_compile_fail, ['UnsafeInfered10', ''])
test('UnsafeInfered11',
     [ extra_clean(['UnsafeInfered11_A.hi', 'UnsafeInfered11_A.o']) ],
     multimod_compile_fail, ['UnsafeInfered11', ''])

# test should fail as unsafe and we made warn unsafe + -Werror
test('UnsafeInfered12', normal, compile_fail, [''])

# Mixed tests
test('Mixed01', normal, compile_fail, [''])
test('Mixed02', normal, compile_fail, [''])
test('Mixed03', normal, compile_fail, [''])

