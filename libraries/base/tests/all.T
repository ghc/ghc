
test('readFloat', exit_code(1), compile_and_run, [''])
test('enumDouble', normal, compile_and_run, [''])
test('enumRatio', normal, compile_and_run, [''])
test('tempfiles', normal, compile_and_run, [''])
test('fixed', normal, compile_and_run, [''])
test('quotOverflow', normal, compile_and_run, [''])
test('assert', exit_code(1), compile_and_run, ['-fno-ignore-asserts'])
test('CPUTime001', normal, compile_and_run, [''])
test('readLitChar',   normal, compile_and_run, [''])
test('unicode001',
     if_platform('i386-unknown-openbsd', expect_fail),
     compile_and_run,
     [''])
test('unicode002',
     if_platform('i386-unknown-openbsd', expect_fail),
     compile_and_run,
     [''])
test('data-fixed-show-read', normal, compile_and_run, [''])
test('showDouble', normal, compile_and_run, [''])
test('hash001', normal, compile_and_run, [''])
test('lexNum', normal, compile_and_run, [''])
test('take001', extra_run_opts('1'), compile_and_run, [''])
test('genericNegative001', extra_run_opts('-1'), compile_and_run, [''])
test('ix001', normal, compile_and_run, [''])

# need to add -K64m to the compiler opts, so that GHCi gets it too
test('ioref001',
     [skip_if_fast,extra_run_opts('+RTS -K64m -RTS')],
     compile_and_run,
     ['+RTS -K64m -RTS'])

