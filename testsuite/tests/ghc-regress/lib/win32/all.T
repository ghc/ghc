# This isn't a very good test to run automatically at the moment, since
# it doesn't terminate

def set_opts( opts ):
  opts.extra_hc_opts = '-package lang -package win32'
  opts.skip = 1

#  if config.platform != "i386-unknown-mingw32" and \
#     config.platform != "i386-unknown-cygwin32":
#	  opts.skip = 1

test('win32001', set_opts, compile_and_run, [''])
test('win32002', normal, compile_and_run, ['-package Win32'])
