test('stm046', [], compile_and_run, [''])
test('stm047', [], compile_and_run, [''])
test('stm048', [], compile_and_run, [''])

# The above tests don't require 'stm', but the ones below do.
setTestOpts(reqlib('stm'))

test('stm049', [reqlib('random')], compile_and_run, ['-package stm'])
test('stm050', [extra_run_opts('10000')], compile_and_run, ['-package stm'])

test('stm052', [reqlib('random')], compile_and_run, ['-package stm'])

test('stm053', [ reqlib('random'),
                 only_ways(['threaded1','threaded2']),
                 when(platform('i386-unknown-mingw32'), skip) ],
               compile_and_run, ['-package stm'])
test('stm054', normal, compile_and_run, ['-package stm'])
test('stm055', exit_code(1), compile_and_run, ['-package stm'])
test('stm056', only_ways(['threaded1','threaded2']),
                compile_and_run, ['-package stm'])

test('stm060', normal, compile_and_run, ['-package stm'])
test('stm061', normal, compile_and_run, ['-package stm'])
test('stm062', normal, compile_and_run, ['-package stm'])
test('stm063', when(fast(),skip), compile_and_run, ['-package stm'])
test('T2411', ignore_stdout, compile_and_run, ['-package stm'])
test('T3049', normal, compile_and_run, ['-package stm'])
test('T4057', normal, compile_and_run, ['-package stm'])
test('stm064', normal, compile_and_run, ['-package stm'])
test('stm065', normal, compile_and_run, ['-package stm'])
test('cloneTChan001', normal, compile_and_run, ['-package stm'])
test('T14171', exit_code(1), compile_and_run, ['-package stm'])
