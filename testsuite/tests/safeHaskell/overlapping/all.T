# overlapping tests Safe Haskell's handling of overlapping instances.

# Just do the normal way, SafeHaskell is all in the frontend
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('SH_Overlap1',
    [ extra_clean(['SH_Overlap1_A.hi', 'SH_Overlap1_A.o',
                   'SH_Overlap1_B.hi', 'SH_Overlap1_B.o']) ],
    multimod_compile_fail, ['SH_Overlap1', ''])

test('SH_Overlap2',
    [ extra_clean(['SH_Overlap2_A.hi', 'SH_Overlap2_A.o',
                   'SH_Overlap2_B.hi', 'SH_Overlap2_B.o']) ],
    multimod_compile_fail, ['SH_Overlap2', ''])

test('SH_Overlap3',
    [ extra_clean(['SH_Overlap3_A.hi', 'SH_Overlap3_A.o',
                   'SH_Overlap3_B.hi', 'SH_Overlap3_B.o']) ],
    multimod_compile, ['SH_Overlap3', ''])

test('SH_Overlap4',
    [ extra_clean(['SH_Overlap4_A.hi', 'SH_Overlap4_A.o',
                   'SH_Overlap4_B.hi', 'SH_Overlap4_B.o']) ],
    multimod_compile, ['SH_Overlap4', ''])

test('SH_Overlap5',
    [ extra_clean(['SH_Overlap5_A.hi', 'SH_Overlap5_A.o',
                   'SH_Overlap5_B.hi', 'SH_Overlap5_B.o']) ],
    multimod_compile_fail, ['SH_Overlap5', ''])

test('SH_Overlap6',
    [ extra_clean(['SH_Overlap6_A.hi', 'SH_Overlap6_A.o',
                   'SH_Overlap6_B.hi', 'SH_Overlap6_B.o']) ],
    multimod_compile_fail, ['SH_Overlap6', ''])

test('SH_Overlap7',
    [ extra_clean(['SH_Overlap7_A.hi', 'SH_Overlap7_A.o',
                   'SH_Overlap7_B.hi', 'SH_Overlap7_B.o']) ],
    multimod_compile_fail, ['SH_Overlap7', '-Werror'])

test('SH_Overlap8',
    [ extra_clean(['SH_Overlap8_A.hi', 'SH_Overlap8_A.o']) ],
    multimod_compile, ['SH_Overlap8', ''])

test('SH_Overlap9',
    [ extra_clean(['SH_Overlap9_A.hi', 'SH_Overlap9_A.o',
                   'SH_Overlap9_B.hi', 'SH_Overlap9_B.o']) ],
    multimod_compile, ['SH_Overlap9', '-Werror'])

test('SH_Overlap10',
    [ extra_clean(['SH_Overlap10_A.hi', 'SH_Overlap10_A.o',
                   'SH_Overlap10_B.hi', 'SH_Overlap10_B.o']) ],
    multimod_compile, ['SH_Overlap10', '-Werror'])

test('SH_Overlap11',
    [ extra_clean(['SH_Overlap11_A.hi', 'SH_Overlap11_A.o',
                   'SH_Overlap11_B.hi', 'SH_Overlap11_B.o']) ],
    multimod_compile, ['SH_Overlap11', ''])

