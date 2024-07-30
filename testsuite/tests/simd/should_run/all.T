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

  # Ensure we set the CPU features we have available.
  #
  # This is especially important with the LLVM backend, as LLVM can otherwise
  # produce ABI-incompatible code, e.g. when compiling usage of YMM registers
  # with or without -mavx2.
  , when(have_cpu_feature('sse4_1'), extra_hc_opts('-msse4'))
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


test('T22187', [],compile,[''])
test('T22187_run', [],compile_and_run,[''])
test('T25062_V16', [], compile_and_run, [''])
test('T25062_V32', [ unless(have_cpu_feature('avx2'), skip)
                   , only_ways(llvm_ways) # SIMD NCG TODO: support 256 bit wide vectors
                   ]
                 , compile_and_run, [''])
test('T25062_V64', [ unless(have_cpu_feature('avx512f'), skip)
                   , only_ways(llvm_ways) # SIMD NCG TODO: support 512 bit wide vectors
                   ]
                 , compile_and_run, [''])
