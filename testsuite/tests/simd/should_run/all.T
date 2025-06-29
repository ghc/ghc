setTestOpts(
  # Currently, the only GHC backends to support SIMD are:
  #   - the X86 NCG
  #   - the RISC-V 64 NCG
  #   - LLVM (any architecture)
  [ unless(arch('x86_64') or arch('riscv64'), only_ways(llvm_ways))
  , req_basic_simd_cpu
  , when(unregisterised(), skip)
  , when(arch('wasm32'), skip)
  , js_skip
  , omit_ghci
  ])

# The code generator may emit different instructions depending on
# the architecture options (e.g. SSE2 vs SSE4.1 vs AVX).
# Ideally, all ISA levels should be tested, but doing so would
# require changing the testing infrastructure.
# For now, we just test the two ISA levels:

# The "baseline ISA" (SSE2 on x86) and "everything we have".
def riscvVlen():
    if have_cpu_feature('zvl128b'):
        return 128
    elif have_cpu_feature('zvl256b'):
        return 256
    elif have_cpu_feature('zvl512b'):
        return 512
    else:
        raise Exception("Vector extension not supported by CPU or VLEN too small.")

# Ensure we set the CPU features we have available.
#
# This is especially important with the LLVM backend, as LLVM can otherwise
# produce ABI-incompatible code, e.g. when compiling usage of YMM registers
# with or without -mavx2.
setTestOpts([
    # TODO: -optc and -opta should not be required, but provided by the GHC pipeline
    when(arch('riscv64'), extra_hc_opts('-mriscv-vlen' + str(riscvVlen()) + " -optc=-march=rv64gv -opta=-march=rv64gv"))
  ])

test('simd_insert_baseline', [], compile_and_run, [''])
test('simd_insert_array_baseline', [], compile_and_run, [''])
test('int8x16_basic_baseline', [], compile_and_run, [''])
test('int16x8_basic_baseline', [], compile_and_run, [''])
test('int32x4_basic_baseline', [], compile_and_run, [''])
test('int64x2_basic_baseline', [], compile_and_run, [''])
test('word8x16_basic_baseline', [], compile_and_run, [''])
test('word16x8_basic_baseline', [], compile_and_run, [''])
test('word32x4_basic_baseline', [], compile_and_run, [''])
test('word64x2_basic_baseline', [], compile_and_run, [''])
test('floatx4_arith_baseline', [], compile_and_run, [''])
test('doublex2_arith_baseline', [], compile_and_run, [''])
test('int8x16_arith_baseline', [], compile_and_run, [''])
test('int16x8_arith_baseline', [], compile_and_run, [''])
test('int32x4_arith_baseline', [], compile_and_run, [''])
test('int64x2_arith_baseline', [], compile_and_run, [''])
test('word8x16_arith_baseline', [], compile_and_run, [''])
test('word16x8_arith_baseline', [], compile_and_run, [''])
test('word32x4_arith_baseline', [], compile_and_run, [''])
test('word64x2_arith_baseline', [], compile_and_run, [''])
test('int8x16_shuffle_baseline', [], compile_and_run, [''])
test('int16x8_shuffle_baseline', [], compile_and_run, [''])
test('int32x4_shuffle_baseline', [], compile_and_run, [''])
test('int64x2_shuffle_baseline', [], compile_and_run, [''])

test('T25658', [], compile_and_run, ['']) # #25658 is a bug with SSE2 code generation
test('T25659', [], compile_and_run, [''])

# Ensure we set the CPU features we have available.
#
# This is especially important with the LLVM backend, as LLVM can otherwise
# produce ABI-incompatible code, e.g. when compiling usage of YMM registers
# with or without -mavx2.
setTestOpts(
  [ when(have_cpu_feature('sse4_2'), extra_hc_opts('-msse4.2'))
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
                , when(arch('x86_64') and not have_cpu_feature('avx'), skip)
                ]
              , multimod_compile_and_run, ['simd009', ''])
test('simd010', [], compile_and_run, [''])
test('simd011', [ req_fma_cpu
                , extra_hc_opts('-mfma')
                ], compile_and_run, [''])
test('simd012', [], compile_and_run, [''])
test('simd013',
     [ req_c
     , unless((arch('x86_64') or arch('riscv64')), skip)
      # ^ Because the C file uses architecture-dependent intrinsics.
      # Requires GCC >= 14 on RISC-V.
     ],
     compile_and_run, ['simd013C.c'])
test('simd014',
     [ unless(arch('x86_64') or arch('riscv64'), skip) ],
        # ^ the test uses hand-written low-level Cmm which makes use
        # of the XMM4 register, which may not be mapped to a real machine
        # register on -x86 architectures.
     compile_and_run, ['simd014Cmm.cmm'])

test('simd_insert', [], compile_and_run, [''])
test('simd_insert_array', [], compile_and_run, [''])
test('int8x16_basic', [], compile_and_run, [''])
test('int16x8_basic', [], compile_and_run, [''])
test('int32x4_basic', [], compile_and_run, [''])
test('int64x2_basic', [], compile_and_run, [''])
test('word8x16_basic', [], compile_and_run, [''])
test('word16x8_basic', [], compile_and_run, [''])
test('word32x4_basic', [], compile_and_run, [''])
test('word64x2_basic', [], compile_and_run, [''])
test('floatx4_arith', [], compile_and_run, [''])
test('doublex2_arith', [], compile_and_run, [''])
test('floatx4_fma', [ unless(have_cpu_feature('fma'), skip)
                    , extra_hc_opts('-mfma')
                    ], compile_and_run, [''])
test('doublex2_fma', [ unless(have_cpu_feature('fma'), skip)
                     , extra_hc_opts('-mfma')
                     ], compile_and_run, [''])
test('int8x16_arith', [], compile_and_run, [''])
test('int16x8_arith', [], compile_and_run, [''])
test('int32x4_arith', [], compile_and_run, [''])
test('int64x2_arith', [], compile_and_run, [''])
test('word8x16_arith', [], compile_and_run, [''])
test('word16x8_arith', [], compile_and_run, [''])
test('word32x4_arith', [], compile_and_run, [''])
test('word64x2_arith', [], compile_and_run, [''])
test('int8x16_shuffle', [], compile_and_run, [''])
test('int16x8_shuffle', [], compile_and_run, [''])
test('int32x4_shuffle', [], compile_and_run, [''])
test('int64x2_shuffle', [], compile_and_run, [''])

test('T22187', [],compile,[''])
test('T22187_run', [],compile_and_run,[''])
test('T25062_V16', [], compile_and_run, [''])
test('T25561', [], compile_and_run, [''])

# Even if the CPU we run on doesn't support *executing* those tests we should try to
# compile them.
# Currently even for compilation we only support 256+ bit on x86 and riscv64
only_V32_plus_compilation_support = unless(arch('x86_64') or arch('riscv64'), skip)

test('T25062_V32'
    ,   [ when(arch('x86_64'), extra_hc_opts('-mavx2'))
    ,     when(arch('riscv64'), extra_hc_opts('-mriscv-vlen=256'))
        , unless(arch('riscv64'), only_ways(llvm_ways)) # SIMD NCG TODO: support 256+ bit wide vectors on more architectures
        , only_V32_plus_compilation_support
        ]
    , compile_and_run if (have_cpu_feature('avx2') or have_cpu_feature('zvl256b')) else compile
    , [''])

test('T25062_V64'
    ,   [ when(arch('x86_64'), extra_hc_opts('-mavx512f'))
    ,     when(arch('riscv64'), extra_hc_opts('-mriscv-vlen=512'))
        , unless(arch('riscv64'), only_ways(llvm_ways)) # SIMD NCG TODO: support 512+ bit wide vectors on more architectures
        , only_V32_plus_compilation_support
        ]
    , compile_and_run if (have_cpu_feature('avx512f') or have_cpu_feature('zvl512b')) else compile
    , [''])

test('T25169', [], compile_and_run, [''])
test('T25455', [], compile_and_run, [''])
test('T25486', [], compile_and_run, [''])

# TODO: Remove debug args
test('VectorCCallConv', [req_c, unless(arch('riscv64'), skip)], compile_and_run, ['-dppr-debug -ddump-to-file -ddump-cmm -ddump-asm -O0 -g -optc=-g -optc=-save-temps -optc=-fverbose-asm -optc=-O VectorCCallConv_c.c'])
