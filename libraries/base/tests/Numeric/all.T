test('num001', normal, compile_and_run, [''])
test('num002', normal, compile_and_run, [''])
test('num003', normal, compile_and_run, [''])
test('num004', normal, compile_and_run, [''])
test('num005', normal, compile_and_run, [''])
test('num006', normal, compile_and_run, [''])
test('num007', normal, compile_and_run, [''])
test('num008', normal, compile_and_run, [''])


test('num009', [ when(fast(), skip)
               # , when(wordsize(32), expect_broken(15062))
               , when(platform('powerpc64le-unknown-linux'), expect_broken(13634))],
             compile_and_run, [opts])
test('num010',
     when(platform('i386-apple-darwin'), expect_broken_for(7043, ['ghci'])),
     compile_and_run,
     [''])
test('sqrt', normal, compile_and_run, [''])
