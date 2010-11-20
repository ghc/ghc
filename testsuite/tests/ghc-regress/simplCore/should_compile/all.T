test('simpl001', normal, compile, [''])
test('simpl002', normal, compile, [''])
test('simpl003', normal, compile, [''])
test('simpl004', only_compiler_types(['ghc']), compile, [''])
test('simpl005', normal, compile, [''])

# Missing:
# test('simpl006', normal, compile, [''])

test('simpl007', normal, compile, [''])
# Missing: simpl008
test('simpl009', normal, compile, [''])
test('simpl010', normal, compile, [''])
test('simpl011', normal, compile, [''])
test('simpl012', normal, compile, [''])
test('simpl013', normal, compile, [''])
test('simpl014', normal, compile, [''])
test('simpl015', only_ways(['optc','optasm']), compile, [''])
test('simpl016', normal, compile, [''])
test('simpl017', normal, compile_fail, [''])
test('simpl018', normal, compile, [''])
test('simpl019', normal, compile, [''])
test('simpl020',
     extra_clean(['Simpl020_A.hi', 'Simpl020_A.o']),
     multimod_compile,
     ['simpl020', '-v0 -XFlexibleInstances -XUndecidableInstances'])

test('simpl-T1370', normal, compile, [''])
test('T2520', normal, compile, [''])


test('spec001', skip_if_fast, compile, [''])
test('spec002', normal, compile, [''])
test('spec003', normal, compile, [''])

test('rule1', normal, compile, [''])
test('rule2', only_ways(['optc','optasm']), compile, ['-dsuppress-uniques'])

test('dfun-loop', normal, compile, [''])
test('strict-float', normal, compile, [''])
test('T3118', normal, compile, [''])
test('T4203', normal, compile, [''])

# With -prof -fvia-C, this test makes gcc go out to lunch, and the
# testsuite driver times out.
test('T3016', [ skip_if_fast, omit_ways(['optc','profc']) ], compile, [''])

test('T1647', normal, compile, ['-fdicts-strict -dcore-lint'])

test('T3234',
     only_ways(['optc','optasm']),
     compile,
     ['-ddump-simpl-stats -dsuppress-uniques'])

test('T3717', only_ways(['optc','optasm']), 
              compile, 
              ['-ddump-simpl -dsuppress-uniques'])

test('spec-inline', only_ways(['optc','optasm']), 
              	    compile, 
              	    ['-O2 -ddump-simpl -dsuppress-uniques'])

test('T3055',
     extra_clean(['T3055.hi', 'T3055.o', 'T3055.simpl']),
     run_command,
     ['$MAKE -s --no-print-directory T3055'])

test('T4306',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4306'])

test('T4201',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4201'])

test('T3772',
#      only_ways(['optc','optasm']), 
     extra_clean(['T3772_A.hi', 'T3772_A.o']),
     run_command, ['$MAKE -s --no-print-directory T3772'])

test('T3831', normal, compile, [''])
test('T4345', normal, compile, [''])
test('T4398', normal, compile, [''])
