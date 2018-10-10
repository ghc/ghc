# Test +RTS -G1 here (it isn't tested anywhere else)
setTestOpts(unless(fast(), extra_ways(['g1'])))

test('cgrun001', normal, compile_and_run, [''])
test('cgrun002', normal, compile_and_run, [''])
test('cgrun003', normal, compile_and_run, [''])
test('cgrun004', normal, compile_and_run, [''])
test('cgrun005', normal, compile_and_run, [''])
test('cgrun006', normal, compile_and_run, [''])
test('cgrun007', normal, compile_and_run, [''])
test('cgrun008', normal, compile_and_run, [''])
test('cgrun009', normal, compile_and_run, [''])
test('cgrun010', normal, compile_and_run, [''])
test('cgrun011', normal, compile_and_run, [''])
test('cgrun012', normal, compile_and_run, [''])
test('cgrun013', normal, compile_and_run, [''])
test('cgrun014', normal, compile_and_run, [''])
test('cgrun015', normal, compile_and_run, [''])
test('cgrun016', exit_code(1), compile_and_run, [''])
test('cgrun017', normal, compile_and_run, [''])
test('cgrun018', normal, compile_and_run, [''])
test('cgrun019', normal, compile_and_run, [''])
test('cgrun020', normal, compile_and_run, [''])

# cgrun021 does some GC, so let's use it to test GC parameters
test('cgrun021', extra_ways(['nursery_chunks']), compile_and_run, [''])

test('cgrun022', normal, compile_and_run, [''])
test('cgrun024', normal, compile_and_run, [''])
test('cgrun025',
     [reqlib('regex-compat'), extra_run_opts('cgrun025.hs'), exit_code(1)],
     compile_and_run, ['-package regex-compat'])
test('cgrun026', normal, compile_and_run, [''])
test('cgrun027', normal, compile_and_run, [''])
test('cgrun028', normal, compile_and_run, [''])
test('cgrun031', normal, compile_and_run, [''])
test('cgrun032', normal, compile_and_run, [''])
test('cgrun033', normal, compile_and_run, [''])
test('cgrun034', normal, compile_and_run, [''])
test('cgrun035', normal, compile_and_run, [''])
test('cgrun036', normal, compile_and_run, [''])
test('cgrun037', normal, compile_and_run, [''])
test('cgrun038', normal, compile_and_run, [''])
test('cgrun039', normal, compile_and_run, [''])
test('cgrun040', normal, compile_and_run, [''])
test('cgrun043', normal, compile_and_run, [''])
test('cgrun044', normal, compile_and_run, [''])
test('cgrun045', exit_code(1), compile_and_run, [''])
test('cgrun046', normal, compile_and_run, [''])
test('cgrun047', normal, compile_and_run, [''])
test('cgrun048', normal, compile_and_run, [''])
test('cgrun049', normal, compile_and_run, ['-funbox-strict-fields'])
test('cgrun050', normal, compile_and_run, [''])
test('cgrun051', [exit_code(1)], compile_and_run, [''])
test('cgrun052', only_ways(['optasm']), compile_and_run, ['-funbox-strict-fields'])
test('cgrun053', normal, compile_and_run, [''])
test('cgrun054', normal, compile_and_run, [''])
test('cgrun055', normal, compile_and_run, [''])
test('cgrun056', normal, compile_and_run, [''])
test('cgrun057', [ only_ways(prof_ways),
                   extra_run_opts('+RTS -xc') ], compile_and_run, [''])
test('cgrun058', normal, compile_and_run, [''])
test('cgrun059', exit_code(1), compile_and_run, [''])
test('cgrun060',
     extra_run_opts('+RTS -K64k -RTS'),
     compile_and_run, [''])
test('cgrun061', normal, compile_and_run, [''])
test('cgrun062', normal, compile_and_run, [''])
test('cgrun063', normal, compile_and_run, [''])
test('cgrun064', normal, compile_and_run, [''])
test('cgrun065', normal, compile_and_run, [''])
test('cgrun066', normal, compile_and_run, [''])
test('cgrun067', [extra_files(['Cgrun067A.hs'])], compile_and_run, [''])
test('cgrun068', reqlib('random'), compile_and_run, [''])
test('cgrun069', omit_ways(['ghci']), multi_compile_and_run,
                 ['cgrun069', [('cgrun069_cmm.cmm', '')], ''])
test('cgrun070', normal, compile_and_run, [''])
test('cgrun071', normal, compile_and_run, [''])
test('cgrun072', normal, compile_and_run, [''])
test('cgrun075', normal, compile_and_run, [''])
test('cgrun076', normal, compile_and_run, [''])

test('T1852', normal, compile_and_run, [''])
test('T1861', extra_run_opts('0'), compile_and_run, [''])
test('T2080', normal, compile_and_run, [''])
test('T2838', normal, compile_and_run, [''])
test('T3207', normal, compile_and_run, [''])
test('T3561', normal, compile_and_run, [''])
test('T3677', extra_run_opts('+RTS -K8k -RTS'), compile_and_run, [''])
test('T4441', normal, compile_and_run, [''])
test('T5149', omit_ways(['ghci']), multi_compile_and_run,
                 ['T5149', [('T5149_cmm.cmm', '')], ''])
test('T5129',
     # The bug is in simplifier when run with -O1 and above, so only run it
     # optimised, using any backend.
     only_ways(['optasm']),
     compile_and_run, [''])
test('T5626', exit_code(1), compile_and_run, [''])
test('T5747', when(arch('i386'), extra_hc_opts('-msse2')), compile_and_run, ['-O2'])
test('T5785', normal, compile_and_run, [''])
test('setByteArray', normal, compile_and_run, [''])
test('compareByteArrays', normal, compile_and_run, [''])

test('T6146', normal, compile_and_run, [''])
test('T5900', normal, compile_and_run, [''])
test('T7163', normal, compile_and_run, [''])

# T7319 now gives the same results in every 'way', so treat it as normal
test('T7319', exit_code(1), compile_and_run, [''])

# Gives different results when optimised, so restrict to just one way
#test('T7319', [ extra_ways(['prof']), only_ways(['prof']), exit_code(1),
#                req_profiling,
#                extra_hc_opts('-fprof-auto'),
#                extra_run_opts('+RTS -xc') ], compile_and_run, [''])

test('Word2Float32', unless(wordsize(32), skip), compile_and_run, [''])
test('Word2Float64', unless(wordsize(64), skip), compile_and_run, [''])

test('T7361', normal, compile_and_run, [''])
test('T7600', normal, compile_and_run, [''])
test('T8103', only_ways(['normal']), compile_and_run, [''])
test('T7953', reqlib('random'), compile_and_run, [''])
test('T8256', normal, compile_and_run, ['-dcore-lint -O1'])
test('T6084',normal, compile_and_run, ['-O2'])
test('CgStaticPointers', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, [''])
test('CgStaticPointersNoFullLazyness', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, ['-O -fno-full-laziness'])
test('StaticArraySize', [when(doing_ghci(), extra_hc_opts('-fobject-code'))],
     compile_and_run, ['-O2'])
test('StaticByteArraySize', normal, compile_and_run, ['-O2'])
test('CopySmallArray', normal, compile_and_run, [''])
test('CopySmallArrayStressTest', reqlib('random'), compile_and_run, [''])
test('SizeOfSmallArray', normal, compile_and_run, [''])
test('T9001', normal, compile_and_run, [''])
test('T9013', omit_ways(['ghci']),  # ghci doesn't support unboxed tuples
     compile_and_run, [''])
test('T9340', normal, compile_and_run, [''])
test('cgrun074', normal, compile_and_run, [''])
test('CmmSwitchTest32', unless(wordsize(32), skip), compile_and_run, [''])
test('CmmSwitchTest64', unless(wordsize(64), skip), compile_and_run, [''])
# Skipping WAY=ghci, because it is not broken.
test('T10245', normal, compile_and_run, [''])
test('T10246', normal, compile_and_run, [''])
test('T9533', normal, compile_and_run, [''])
test('T9533b', normal, compile_and_run, [''])
test('T9533c', normal, compile_and_run, [''])
test('T10414', [only_ways(['threaded2']), extra_ways(['threaded2']), req_smp],
     compile_and_run, ['-feager-blackholing'])
test('T10521', normal, compile_and_run, [''])
test('T10521b', normal, compile_and_run, [''])
test('T10870', when(wordsize(32), skip), compile_and_run, [''])
test('PopCnt', omit_ways(['ghci']), multi_compile_and_run,
                 ['PopCnt', [('PopCnt_cmm.cmm', '')], ''])
test('T12059', normal, compile_and_run, [''])
test('T12433', normal, compile_and_run, [''])
test('T12622', normal, multimod_compile_and_run, ['T12622', '-O'])
test('T12757', normal, compile_and_run, [''])
test('T12855', normal, compile_and_run, [''])
test('T9577', [ unless(arch('x86_64') or arch('i386'),skip),
                when(opsys('darwin'), expect_broken(12937)),
                when(opsys('mingw32'), expect_broken(12965)),
                only_ways(['normal']) ], compile_and_run, [''])

test('T13425', normal, compile_and_run, ['-O'])
test('castFloatWord', normal, compile_and_run, ['-dcmm-lint'])
test('T13825-unit',
     extra_run_opts('"' + config.libdir + '"'),
     compile_and_run,
     ['-package ghc'])
test('T14619', normal, compile_and_run, [''])
test('T14754', normal, compile_and_run, [''])
test('T14346', only_ways(['threaded1','threaded2']), compile_and_run, ['-O -threaded'])
test('T14251', [expect_broken_for(14251, [''])],
     compile_and_run, [''])

# These actually used to fail with all optimisation settings, but adding -O just
# to make sure
test('T15696_1', normal, compile_and_run, ['-O'])
test('T15696_2', normal, compile_and_run, ['-O'])
# This requires -O
test('T15696_3', normal, compile_and_run, ['-O'])
