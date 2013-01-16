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
test('simpl015', only_ways(['optasm']), compile, [''])
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
test('rule2', only_ways(['optasm']), compile, ['-dsuppress-uniques'])
test('T7162', normal, compile, [''])

test('dfun-loop', normal, compile, [''])
test('strict-float', normal, compile, [''])
test('T3118', normal, compile, [''])
test('T4203', normal, compile, [''])

# With -prof -fvia-C, this test makes gcc go out to lunch, and the
# testsuite driver times out.
test('T3016', skip_if_fast, compile, [''])

test('T1647', normal, compile, ['-fdicts-strict -dcore-lint'])

test('T3234',
     only_ways(['optasm']),
     compile,
     ['-ddump-simpl-stats -dsuppress-uniques'])

test('T3717', only_ways(['optasm']),
              compile,
              ['-ddump-simpl -dsuppress-uniques'])

test('spec-inline', only_ways(['optasm']),
              	    compile,
              	    ['-O2 -ddump-simpl -dsuppress-uniques'])
test('T4908', only_ways(['optasm']),
              compile,
              ['-O2 -ddump-simpl -dsuppress-uniques'])

test('T4930', [if_compiler_lt('ghc', '7.1', expect_fail),
               only_ways(['optasm'])],
              compile,
              ['-O -ddump-simpl -dsuppress-uniques'])

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
#      only_ways(['optasm']),
     extra_clean(['T3772_A.hi', 'T3772_A.o']),
     run_command, ['$MAKE -s --no-print-directory T3772'])

test('EvalTest',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory EvalTest'])

test('T3831', normal, compile, [''])
test('T4345', normal, compile, [''])
test('T4398', normal, compile, [''])

test('T4903',
     extra_clean(['T4903a.hi', 'T4903a.o']),
     run_command,
     ['$MAKE -s --no-print-directory T4903'])

test('T4918',
     extra_clean(['T4918a.hi', 'T4918a.o']),
     run_command,
     ['$MAKE -s --no-print-directory T4918'])

test('T4945',
     if_compiler_lt('ghc', '7.1', expect_fail),
     run_command,
     ['$MAKE -s --no-print-directory T4945'])

test('T4957',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T4957'])

test('T5168',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T5168'])

test('T5329', normal, compile, [''])
test('T5303', reqlib('mtl'), compile, [''])   # Coercion-optimiation test
test('T5342', normal, compile, [''])   # Lint error with -prof
test('T5359a', normal, compile, [''])  # Lint error with -O (OccurAnal)
test('T5359b', normal, compile, [''])  # Lint error with -O (OccurAnal)
test('T5458', normal, compile, [''])
test('simpl021',
     extra_clean(['Simpl021A.hi', 'Simpl021A.o',
                  'Simpl021B.hi', 'Simpl021B.o']),
     run_command,
     ['$MAKE -s --no-print-directory simpl021'])
test('T5327', normal, run_command, ['$MAKE -s --no-print-directory T5327'])
test('T5623', normal, run_command, ['$MAKE -s --no-print-directory T5623'])
test('T5658b',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T5658b'])
test('T5776',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T5776'])
test('T4138',
     extra_clean(['T4138_A.hi', 'T4138_A.o', 'T4138.simpl']),
     run_command,
     ['$MAKE -s --no-print-directory T4138'])
test('T7088', normal, compile, [''])
test('T6082-RULE', normal, compile, [''])
test('T7165',
     extra_clean(['T7165a.hi', 'T7165a.o']),
     run_command,
     ['$MAKE -s --no-print-directory T7165'])
test('T7287', normal, compile, [''])
test('T7360', only_ways(['optasm']), compile, ['-ddump-simpl -dsuppress-uniques'])
test('T5366',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory T5366'])
