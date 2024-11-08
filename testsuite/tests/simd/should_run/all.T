setTestOpts(
  # Currently, the only GHC backends to support SIMD are:
  #   - the X86 NCG
  #   - LLVM (any architecture)
  [ unless(arch('x86_64'), only_ways(llvm_ways))

  # Architectures which support at least 128 bit wide SIMD vectors:
  #  - X86 with SSE4.1
  #  - AArch64 (which includes NEON)
  #  - PowerPC with AltiVec (not currently supported)
  , unless(arch('aarch64') or have_cpu_feature('sse4_1') or have_cpu_feature('avx'), skip)
  , when(unregisterised(), skip)
  , when(arch('wasm32'), skip)
  , js_skip
  ])

test('simd_insert_baseline', [], compile_and_run, [''])
test('simd_insert_array_baseline', [], compile_and_run, [''])

# Ensure we set the CPU features we have available.
#
# This is especially important with the LLVM backend, as LLVM can otherwise
# produce ABI-incompatible code, e.g. when compiling usage of YMM registers
# with or without -mavx2.
setTestOpts(
  [ when(have_cpu_feature('sse4_1'), extra_hc_opts('-msse4'))
  , when(have_cpu_feature('avx'), extra_hc_opts('-mavx'))
  , when(have_cpu_feature('avx2'), extra_hc_opts('-mavx2'))
  , when(have_cpu_feature('avx512f'), extra_hc_opts('-mavx512f'))
  ])

test('simd000', [], compile_and_run, [''])
test('simd001', [], compile_and_run, [''])
test('simd002', [], compile_and_run, [''])
test('simd003', [], compile_and_run, [''])
test('simd004', [], compile_and_run, ['-O2'])
test('simd005', [], compile_and_run, [''])
test('simd006', [], compile_and_run, [''])
test('simd007', [], compile_and_run, [''])
test('simd008', [], compile_and_run, [''])
test('simd009', [ req_th
                , extra_files(['Simd009b.hs', 'Simd009c.hs'])
                , unless(have_cpu_feature('avx'), skip)
                ]
              , multimod_compile_and_run, ['simd009', ''])
test('simd010', [], compile_and_run, [''])
test('simd011', [ unless(have_cpu_feature('fma'), skip)
                , extra_hc_opts('-mfma')
                ], compile_and_run, [''])
test('simd012', [], compile_and_run, [''])
test('simd013',
     [ req_c
     , unless(arch('x86_64'), skip) # because the C file uses Intel intrinsics
     ],
     compile_and_run, ['simd013C.c'])
test('simd014',
     [ unless(arch('x86_64'), skip) ],
        # ^ the test uses hand-written low-level Cmm which makes use
        # of the XMM4 register, which may not be mapped to a real machine
        # register on non-x86 architectures.
     compile_and_run, ['simd014Cmm.cmm'])

test('simd_insert', [], compile_and_run, [''])
test('simd_insert_array', [], compile_and_run, [''])

test('T22187', [],compile,[''])
test('T22187_run', [],compile_and_run,[''])
test('T25062_V16', [], compile_and_run, [''])

# Even if the CPU we run on doesn't support *executing* those tests we should try to
# compile them.
# Currently even for compilation we only support 256+ bit on x86
only_V32_plus_compilation_support = unless(arch('x86_64'), skip)

test('T25062_V32'
    ,   [ extra_hc_opts('-mavx2')
        , only_ways(llvm_ways) # SIMD NCG TODO: support 256+ bit wide vectors
        , only_V32_plus_compilation_support
        ]
    , compile_and_run if have_cpu_feature('avx2') else compile
    , [''])

test('T25062_V64'
    ,   [ extra_hc_opts('-mavx512f')
        , only_ways(llvm_ways) # SIMD NCG TODO: support 256+ bit wide vectors
        , only_V32_plus_compilation_support
        ]
    , compile_and_run if have_cpu_feature('avx512f') else compile
    , [''])

test('T25169', [], compile_and_run, [''])
test('T25455', [], compile_and_run, [''])
