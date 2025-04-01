test('T6135', normal, compile_and_run, [''])
test('T7689', normal, compile_and_run, [''])
test('T9430', normal, compile_and_run, [''])
test('T4442',
     [when(wordsize(32), expect_broken(15184))],
     compile_and_run, [''])
test('T10481', exit_code(1), compile_and_run, [''])
test('UnalignedAddrPrimOps', normal, compile_and_run, [''])
test('T10678',
     [ collect_stats('bytes allocated',5),
       only_ways(['normal']),
       js_broken(22361)
     ],
     compile_and_run, ['-O'])
test('T11296', normal, compile_and_run, [''])
test('T13825-compile', normal, compile_and_run, [''])
test('T16164', normal, compile_and_run, [''])
test('ShowPrim', normal, compile_and_run, [''])
test('T12492', normal, compile_and_run, [''])

test('ArithInt8', normal, compile_and_run, [''])
test('ArithWord8', normal, compile_and_run, [''])
test('ArithInt16', normal, compile_and_run, [''])
test('ArithWord16', normal, compile_and_run, [''])
test('ArithInt32', normal, compile_and_run, [''])
test('ArithWord32', normal, compile_and_run, [''])

test('CmpInt8', normal, compile_and_run, [''])
test('CmpWord8', normal, compile_and_run, [''])
test('CmpInt16', normal, compile_and_run, [''])
test('CmpWord16', normal, compile_and_run, [''])
test('CmpInt32', normal, compile_and_run, [''])
test('CmpWord32', normal, compile_and_run, [''])

test('ShrinkSmallMutableArrayA', normal, compile_and_run, [''])
test('ShrinkSmallMutableArrayB', normal, compile_and_run, [''])
test('T14664', normal, compile_and_run, [''])
test('CStringLength', normal, compile_and_run, ['-O2'])
test('NonNativeSwitch', normal, compile_and_run, ['-O2'])
test('Sized', [
      # optasm is broken with js backend: #25922
      extra_ways(['optasm','ghci','ghci-opt']) if not js_arch()
        else extra_ways(['ghci','ghci-opt'])
    ], compile_and_run, [''])

test('LevPolyPtrEquality1', normal, compile_and_run, [''])
test('LevPolyPtrEquality2', normal, compile_and_run, [''])

test('UnliftedArray1', normal, compile_and_run, [''])
test('UnliftedArray2', normal, compile_and_run, [''])
test('UnliftedArrayCAS', normal, compile_and_run, [''])
test('UnliftedIOPort', js_broken(22261), compile_and_run, [''])
test('UnliftedMutVar1', normal, compile_and_run, [''])
test('UnliftedMutVar2', normal, compile_and_run, [''])
test('UnliftedMutVar3', normal, compile_and_run, [''])
test('UnliftedMutVarCAS', normal, compile_and_run, [''])
test('UnliftedMVar', normal, compile_and_run, [''])
test('UnliftedSmallArray1', normal, compile_and_run, [''])
test('UnliftedSmallArray2', normal, compile_and_run, [''])
test('UnliftedStablePtr', normal, compile_and_run, [''])
test('UnliftedTVar1', normal, compile_and_run, [''])
test('UnliftedTVar2', normal, compile_and_run, [''])
test('UnliftedWeakPtr', normal, compile_and_run, [''])

test('FMA_Primops'
    , [ when(have_cpu_feature('fma'), extra_hc_opts('-mfma'))
      , js_skip # JS backend doesn't have an FMA implementation
      , when(arch('wasm32'), skip)
      , when(have_llvm(), extra_ways(["optllvm"]))
      ]
     , compile_and_run, [''])
test('FMA_ConstantFold'
    , [ when(have_cpu_feature('fma'), extra_hc_opts('-mfma'))
      , js_skip # JS backend doesn't have an FMA implementation
      , when(arch('wasm32'), skip)
      , expect_broken(21227)
      , when(have_llvm(), extra_ways(["optllvm"]))
      ]
    , compile_and_run, ['-O'])

test('T21624', normal, compile_and_run, [''])
test('T23071',
     [when(opsys('freebsd'), fragile(25751)), ignore_stdout],
     compile_and_run,
     [''])
test('T22710', normal, compile_and_run, [''])
test('T24496'
    , [ when(have_cpu_feature('fma'), extra_hc_opts('-mfma'))
      , js_skip # JS backend doesn't have an FMA implementation
      , when(arch('wasm32'), skip)
      , when(have_llvm(), extra_ways(["optllvm"]))
      ]
    , compile_and_run, ['-O'])
test('T6026', normal, compile_and_run, [''])
