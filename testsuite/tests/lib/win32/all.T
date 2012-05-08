def win_only(opts):
    if config.os != "mingw32" and config.os != "cygwin32":
        opts.skip = 1

# This isn't a very good test to run automatically at the moment, since
# it doesn't terminate
test('win32001', skip, compile_and_run, ['-package lang -package win32'])

test('win32002', win_only, compile_and_run, ['-package Win32'])
