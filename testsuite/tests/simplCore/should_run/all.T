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
test('simplrun011', normal, compile_and_run, ['-fno-worker-wrapper'])

# Really we'd like to run T2486 too, to check that its
# runtime has not gone up, but here I just compile it so that
# the output of -ddump-rules can be compared
test('T2486', normal, compile, [''])
test('T2756', normal, compile_and_run, [''])
test('T3437', extra_run_opts('+RTS -M10m'), compile_and_run, [''])

test('SeqRule', only_ways(['optasm']), compile_and_run, [''])

test('runST', normal, compile_and_run, [''])

test('T3403', normal, compile_and_run, ['-package containers'])
test('T3591', normal, compile_and_run, [''])
test('T4814', normal, compile_and_run, [''])

# Run this test *without* optimisation too
test('T3959', only_ways(['normal','optasm']), compile_and_run, [''])
test('T3983', [only_ways(['normal', 'optasm'])], multimod_compile_and_run,
     ['T3983', ''])
test('T3972', [], compile_and_run, [''])
test('T5315', normal, compile_and_run, [''])
test('T5453', normal, compile_and_run, [''])
test('T5441', [], multimod_compile_and_run, ['T5441', ''])
# This compares Core from integer-gmp
test('T5603', reqlib('integer-gmp'), compile_and_run, [''])
test('T2110', normal, compile_and_run, [''])
test('AmapCoerce', normal, compile_and_run, [''])
test('T16208', normal, compile_and_run, [''])

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

test('T9128', normal, compile_and_run, [''])
test('T9390', normal, compile_and_run, [''])
test('T10830', extra_run_opts('+RTS -K100k -RTS'), compile_and_run, [''])
test('T11172', normal, compile_and_run, [''])
test('T11731', normal, compile_and_run, ['-fspec-constr'])
test('T7611', normal, compile_and_run, [''])
test('T12689broken', expect_broken(12689), compile_and_run, [''])
test('T12689a', normal, compile_and_run, [''])

test('T13172', only_ways(['optasm']), compile_and_run, ['-dcore-lint'])
test('T13227', normal, compile_and_run, [''])
test('T13733', expect_broken(13733), compile_and_run, [''])
test('T13429', normal, compile_and_run, [''])
test('T13429_2', normal, compile_and_run, [''])
test('T13750', normal, compile_and_run, [''])
test('T14178', normal, compile_and_run, [''])
test('T14768', reqlib('vector'), compile_and_run, [''])
test('T14868',
     [when((arch('powerpc64') or arch('powerpc64le')), expect_broken(11261))],
     compile_and_run, [''])
test('T14894', [when((arch('powerpc64') or arch('powerpc64le')), expect_broken(11261))], compile_and_run, [''])
test('T14965', normal, compile_and_run, [''])
test('T15114', only_ways(['optasm']), compile_and_run, [''])
test('T15436', normal, compile_and_run, [''])
test('T15840', normal, compile_and_run, [''])
test('T15840a', normal, compile_and_run, [''])
test('T16066', exit_code(1), compile_and_run, ['-O1'])
test('T17206', exit_code(1), compile_and_run, [''])
test('T17151', [], multimod_compile_and_run, ['T17151', ''])
