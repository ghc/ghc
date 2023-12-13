# Test +RTS -G1 here (it isn't tested anywhere else)
# N.B. Nonmoving collector doesn't support -G1
setTestOpts(unless(fast(), [ extra_ways(['g1']), omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_ghc'])]))

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
     [ omit_ghci, extra_run_opts('cgrun025.hs < /dev/null'), exit_code(1) ],
     compile_and_run, [''])
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
                   fragile(17554),
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
test('cgrun069',
     [ req_cmm, when(arch('wasm32'), fragile(22854))],
     multi_compile_and_run,
     ['cgrun069', [('cgrun069_cmm.cmm', '')], ''])
test('cgrun070', normal, compile_and_run, [''])
test('cgrun071',
     [when(have_cpu_feature('sse4_2'), extra_hc_opts('-msse4.2')),
      when(arch('i386'), expect_broken_for(17247, ['optasm', 'profthreaded', 'profasm', 'dyn', 'threaded2']))],
     compile_and_run, [''])
test('cgrun072', normal, compile_and_run, [''])
test('cgrun075', normal, compile_and_run, [''])
test('cgrun076', normal, compile_and_run, [''])
test('cgrun077', [when(have_cpu_feature('bmi2'), extra_hc_opts('-mbmi2'))], compile_and_run, [''])
test('cgrun078', normal, compile_and_run, [''])
test('cgrun079', normal, compile_and_run, [''])
test('cgrun080', normal, compile_and_run, [''])
test('cas_int', normal, compile_and_run, [''])

test('T1852', normal, compile_and_run, [''])
test('T1861', extra_run_opts('0'), compile_and_run, [''])
test('T2080', normal, compile_and_run, [''])
test('T2838', normal, compile_and_run, [''])
test('T3207', normal, compile_and_run, [''])
test('T3561', normal, compile_and_run, [''])
test('T3677', extra_run_opts('+RTS -K8k -RTS'), compile_and_run, [''])
test('T4441', normal, compile_and_run, [''])
test('T5149', [req_cmm], multi_compile_and_run,
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
test('SizeOfSmallArray', normal, compile_and_run, [''])
test('NewSmallArray', normal, compile_and_run, [''])
test('T9001', normal, compile_and_run, [''])
test('T9013', normal,
     compile_and_run, [''])
test('T9340', normal, compile_and_run, [''])
test('cgrun074', normal, compile_and_run, [''])
test('CmmSwitchTest32', [unless(wordsize(32), skip), req_cmm], compile_and_run, [''])
test('CmmSwitchTest64', [unless(wordsize(64), skip), req_cmm], compile_and_run, [''])
test('T10245', normal, compile_and_run, [''])
test('T10246', normal, compile_and_run, [''])
test('T9533', normal, compile_and_run, [''])
test('T9533b', normal, compile_and_run, [''])
test('T9533c', normal, compile_and_run, [''])
test('T10414', [only_ways(['threaded2']), extra_ways(['threaded2']), req_target_smp, req_ghc_smp, req_ghc_with_threaded_rts],
     compile_and_run, ['-feager-blackholing'])
test('T10521', normal, compile_and_run, [''])
test('T10521b', normal, compile_and_run, [''])
test('T10870', when(wordsize(32), skip), compile_and_run, [''])
test('PopCnt', [req_cmm], multi_compile_and_run, ['PopCnt', [('PopCnt_cmm.cmm', '')], ''])
test('T12059',
  [ js_skip # ByteArrays are always pinned with the JS backend
  ],
  compile_and_run, [''])
test('T12433', normal, compile_and_run, [''])
test('T12622', normal, multimod_compile_and_run, ['T12622', '-O'])
test('T12757', normal, compile_and_run, [''])
test('T12855', normal, compile_and_run, [''])
test('T9577', [ unless(arch('x86_64') or arch('i386'),skip),
                when(opsys('darwin'), expect_broken(12937)),
                when(opsys('mingw32'), expect_broken(12965)),
                when(unregisterised(), skip),
                only_ways(['normal']) ], compile_and_run, [''])

test('T13425', normal, compile_and_run, ['-O'])
test('castFloatWord', normal, compile_and_run, ['-dcmm-lint'])
test('T13825-unit',
     [ extra_run_opts('"' + config.libdir + '"')
     , js_skip # test StgToCmm with JS settings (closure header size is 0)
     ],
     compile_and_run,
     ['-package ghc'])
test('T14619', normal, compile_and_run, [''])
test('T14754', normal, compile_and_run, [''])
test('T14346', only_ways(['threaded1','threaded2']), compile_and_run, ['-O -threaded'])
test('T14251', normal, compile_and_run, [''])

# These actually used to fail with all optimisation settings, but adding -O just
# to make sure
test('T15696_1', normal, compile_and_run, ['-O'])
test('T15696_2', normal, compile_and_run, ['-O'])
# This requires -O
test('T15696_3', normal, compile_and_run, ['-O'])

test('T15892',
     [ ignore_stdout,
       # -G1 is unsupported by the nonmoving GC
       omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_ghc', 'nonmoving_thr_sanity']),
       # we want to do lots of major GC to make the bug more likely to
       # happen, so -G1 -A32k:
       extra_run_opts('+RTS -G1 -A32k -RTS') ],
     compile_and_run, ['-O'])
test('T16617', normal, compile_and_run, [''])
test('T16449_2', exit_code(0), compile_and_run, [''])
test('T16846', [only_ways(['optasm']), exit_code(1)], compile_and_run, [''])

test('T17920', [cmm_src, when(arch('wasm32'), fragile(22854))], compile_and_run, [''])
test('T18527', req_c, compile_and_run, ['T18527FFI.c'])
test('T19149', [req_c,only_ways('sanity')], compile_and_run, ['T19149_c.c'])
test('T20275', normal, compile_and_run, [''])

test('CallConv', [when(unregisterised(), skip),
                  unless(arch('x86_64') or arch('aarch64'), skip),
                  when(arch('x86_64'), extra_hc_opts('CallConv_x86_64.s')),
                  when(arch('aarch64'), extra_hc_opts('CallConv_aarch64.s'))],
     compile_and_run, [''])
test('T21141', normal, compile_and_run, [''])
test('T21186', normal, compile_and_run, [''])
test('T20640a', normal, compile_and_run, [''])
test('T20640b', normal, compile_and_run, [''])
test('T22296',[only_ways(llvm_ways)
              ,unless(arch('x86_64') or arch('aarch64'), skip)],compile_and_run,[''])
test('T22798', normal, compile_and_run, ['-fregs-graph'])
test('CheckBoundsOK', normal, compile_and_run, ['-fcheck-prim-bounds'])
test('OrigThunkInfo', normal, compile_and_run, ['-forig-thunk-info'])

# TODO: Enable more architectures here. N.B. some code generation backends are
# not implemeted correctly (according to
# Note [MO_S_MulMayOflo significant width]) and may require fixing/adjustment.
test('MulMayOflo_full',
     [ extra_files(['MulMayOflo.hs', 'MulMayOflo_cmm.cmm']),
       when(unregisterised(), skip),
       unless(
         arch('aarch64') or arch('x86_64') or arch('i386') or arch('riscv64'),
         expect_broken(23742)
       ),
        ignore_stdout],
     multi_compile_and_run,
     ['MulMayOflo', [('MulMayOflo_full.cmm', '')], ''])
test('T24264run', normal, compile_and_run, [''])
test('T24295a', normal, compile_and_run, ['-O -floopification'])
test('T24295b', normal, compile_and_run, ['-O -floopification -fpedantic-bottoms'])

test('CCallConv',
     [],
     multi_compile_and_run,
     ['CCallConv', [('CCallConv_c.c', '')], ''])
