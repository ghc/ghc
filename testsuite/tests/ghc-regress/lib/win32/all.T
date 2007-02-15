def win_only(opts):
    if config.platform != "i386-unknown-mingw32" and \
       config.platform != "i386-unknown-cygwin32":
	opts.skip = 1

# This isn't a very good test to run automatically at the moment, since
# it doesn't terminate
test('win32001', skip, compile_and_run, ['-package lang -package win32'])

test('win32002', win_only, compile_and_run, ['-package Win32'])
