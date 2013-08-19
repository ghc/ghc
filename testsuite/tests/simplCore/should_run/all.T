# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# Only compile with optimisation
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('simplrun001', normal, compile_and_run, [''])
test('simplrun002', normal, compile_and_run, [''])
test('simplrun003', normal, compile_and_run, [''])
test('simplrun004', normal, compile_and_run, [''])
test('simplrun005', normal, compile_and_run, [''])

test('simplrun007', normal, compile_and_run, [''])
test('simplrun008', normal, compile_and_run, [''])
test('simplrun009', normal, compile_and_run, [''])
test('simplrun010', [extra_run_opts('24 16 8 +RTS -M10m -RTS'),
                     exit_code(251)]
                  , compile_and_run, [''])

# Really we'd like to run T2486 too, to check that its
# runtime has not gone up, but here I just compile it so that
# the output of -ddump-rules can be compared
test('T2486', normal, compile, [''])
test('T2756', normal, compile_and_run, [''])
test('T3437', extra_run_opts('+RTS -M10m'), compile_and_run, [''])

test('SeqRule', only_ways(['optasm']), compile_and_run, [''])

test('T3403', normal, compile_and_run, ['-package containers'])
test('T3591', normal, compile_and_run, [''])
test('T4814', normal, compile_and_run, [''])

# Run this test *without* optimisation too
test('T3959', only_ways(['normal','optasm']), compile_and_run, [''])
test('T3983', [only_ways(['normal','optasm']),
               extra_clean(['T3983_Foo.hi','T3983_Foo.o','T3983_Bar.hi','T3983_Bar.o',])], 
              multimod_compile_and_run, 
              ['T3983',''])
test('T3972', extra_clean(['T3972A.hi', 'T3972A.o']),
              compile_and_run,
              [''])
test('T5315', normal, compile_and_run, [''])
test('T5453', normal, compile_and_run, [''])
test('T5441', extra_clean(['T5441a.o','T5441a.hi']), 
              multimod_compile_and_run, ['T5441',''])
test('T5603', normal, compile_and_run, [''])

# Run these tests *without* optimisation too
test('T5625', [ only_ways(['normal','optasm']), exit_code(1) ], compile_and_run, [''])
test('T5587', [ only_ways(['normal','optasm']), exit_code(1) ], compile_and_run, [''])
test('T5915', only_ways(['normal','optasm']), compile_and_run, [''])
test('T5920', only_ways(['normal','optasm']), compile_and_run, [''])
test('T5997', normal, compile_and_run, [''])
test('T7101', normal, compile_and_run, [''])
test('T7924', exit_code(1), compile_and_run, [''])

# Run this test *without* optimisation too
test('T457', [ only_ways(['normal','optasm']), exit_code(1) ], compile_and_run, [''])
