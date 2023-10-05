test('T6135_should_compile', normal, compile, [''])
test('T16293a', normal, compile, [''])
test('T19851', normal, compile, ['-O'])
test('LevPolyPtrEquality3', normal, compile, [''])
test('LevAddrToAny', normal, compile, [''])
test('UnliftedMutVar_Comp', normal, compile, [''])
test('UnliftedStableName', normal, compile, [''])
test('KeepAliveWrapper', normal, compile, ['-O'])
test('T22152', normal, compile, [''])
test('T22152b', normal, compile, [''])
test('T24160_FMA'
    , [ js_skip # JS backend doesn't have an FMA implementation ]
      # , omit_ghci # fails during compilation phase, remove after !10563
      ]
    , compile, ['-O'])