test('registry001', normal, compile_and_run, [''])

# This isn't a very good test to run automatically at the moment, since
# it doesn't terminate
test('helloworld', skip, compile_and_run, ['-package lang -package win32'])

test('lasterror', normal, compile_and_run, ['-package Win32'])
test('T4452', normal, compile_and_run, ['-package Win32'])
test('PokeTZI', ignore_stdout, compile_and_run, ['-package Win32'])
test('HandleConversion', normal, compile_and_run, ['-package Win32'])
