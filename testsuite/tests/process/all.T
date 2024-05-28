# some platforms use spawnp instead of exec in some cases, resulting
# in spurious error output changes.
normalise_exec = normalise_fun(lambda s: s.replace('posix_spawnp', 'exec'))

test('process001', [req_process], compile_and_run, [''])
test('process002', [fragile_for(16547, concurrent_ways), req_process], compile_and_run, [''])
test('process003', [fragile_for(17245, concurrent_ways), req_process], compile_and_run, [''])
test('process004', [normalise_exec, normalise_exe, req_process], compile_and_run, [''])
test('T1780', [req_process], compile_and_run, [''])
test('process005', [omit_ghci, req_process, when(js_arch(), fragile(24373))], compile_and_run, [''])
test('process006', [req_process], compile_and_run, [''])

test('process007',
     [when(opsys('mingw32'), skip),
      pre_cmd('$MAKE -s --no-print-directory process007_fd'),
      js_broken(22349),
      req_process],
     compile_and_run, [''])
test('process008', [req_process], compile_and_run, [''])

# not the normal way: this test runs processes from multiple threads, and
# will get stuck without the threaded RTS.
test('T3231',
     [only_ways(['threaded1','threaded2']),
      req_process],
     compile_and_run,
     [''])
test('T4198',
     [pre_cmd('{compiler} exitminus1.c -no-hs-main -o exitminus1'),
      js_broken(22349),
      req_process],
     compile_and_run,
     [''])

test('T3994', [only_ways(['threaded1','threaded2']),
               extra_files(['T3994app.hs']),
               pre_cmd('$MAKE -s --no-print-directory T3994app'),
               req_process],
     compile_and_run, [''])
test('T4889',[req_process], compile_and_run, [''])

test('process009', [when(opsys('mingw32'), skip), req_process], compile_and_run, [''])
test('process010', [
    normalise_fun(lambda s: s.replace('illegal operation (Inappropriate ioctl for device)', 'does not exist (No such file or directory)')),
    normalise_exec,
    req_process
], compile_and_run, [''])
test('process011',
     [when(opsys('mingw32'), skip), pre_cmd('{compiler} -no-hs-main -o process011_c process011_c.c'), js_broken(22349), req_process],
     compile_and_run, [''])

test('T8343', [req_process], compile_and_run, [''])
test('processT251', [omit_ghci, req_process], compile_and_run, [''])
