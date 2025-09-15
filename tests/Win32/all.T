test('registry001', unless(opsys('mingw32'), skip), compile_and_run, [''])

# This isn't a very good test to run automatically at the moment, since
# it doesn't terminate
test('helloworld', skip, compile_and_run, ['-package lang -package win32'])

test('lasterror', unless(opsys('mingw32'), skip), compile_and_run, ['-package Win32'])
test('T4452', unless(opsys('mingw32'), skip), compile_and_run, ['-package Win32'])
test('PokeTZI', [unless(opsys('mingw32'), skip), ignore_stdout], compile_and_run, ['-package Win32'])
test('HandleConversion', unless(opsys('mingw32'), skip), compile_and_run, ['-package Win32'])

test('Semaphores', unless(opsys('mingw32'), skip), compile_and_run, ['-threaded -package Win32'])
