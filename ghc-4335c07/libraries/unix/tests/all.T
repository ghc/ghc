
test('signals001',  normal, compile_and_run, ['-package unix -cpp'])
test('signals002', [], compile_and_run, ['-package unix'])
test('fileexist01', normal, compile_and_run, ['-package unix'])

# test #4512
test('forkprocess01', extra_ways(['threaded1_ls']), compile_and_run,
     ['-package unix'])

#
# user001 may fail due to this bug in glibc:
#   http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=466647
#
# Ticket #1487. The glibc implementation of getlogin, which is called by
# getLoginName, requires that a terminal is connected to filedescriptor 0.
# See: https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/getlogin.c
# Therefore we have to omit the 'ghci' way, because it relies on redirecting
# stdin from file.
#
# But getLoginName also fails on GNU/Linux when using a terminal emulator
# that doesn't write login records to /var/run/utmp. Running:
#   $ logname
# should print your login name. If it doesn't, the getLoginName test in user001
# would fail, so we disabled that test.
#
test('user001', omit_ways(['ghci']), compile_and_run, ['-package unix'])
test('resourceLimit', normal, compile_and_run, ['-package unix'])

x86FreeBsdFail = when(platform('i386-unknown-freebsd'), expect_fail)

test('queryfdoption01', [omit_ways(['ghci']), x86FreeBsdFail], compile_and_run,
     ['-package unix'])
test('getEnvironment01', x86FreeBsdFail, compile_and_run, ['-package unix'])
test('getEnvironment02', x86FreeBsdFail, compile_and_run, ['-package unix'])
test('getGroupEntryForName', [x86FreeBsdFail, exit_code(1)], compile_and_run,
     ['-package unix'])
test('getUserEntryForName', [x86FreeBsdFail, exit_code(1)], compile_and_run,
     ['-package unix'])


test('signals004', normal, compile_and_run, ['-package unix'])

if ('threaded1' in config.run_ways):
   only_threaded_ways = only_ways(['ghci','threaded1','threaded2'])
else:
   only_threaded_ways = skip

test('fdReadBuf001', only_threaded_ways, compile_and_run, ['-package unix'])

test('fileStatus',
     extra_clean(['dir', 'regular', 'link-dir', 'link-regular']),
     compile_and_run,
     ['-package unix'])

test('fileStatusByteString',
     extra_clean(['dir', 'regular', 'link-dir', 'link-regular']),
     compile_and_run,
     ['-package unix'])


test('T1185', normal, compile_and_run, ['-package unix'])

# This test fails for me on x86/Linux with a "does not exist" error.
# Running with strace shows it is trying to talk to winbindd (part of
# Samba), so I think the failure has nothing to do with GHC.  Also it
# works on a different machine that doesn't have Samba installed.
#  --SDM 18/05/2010
test('T3816', normal, compile_and_run, ['-package unix'])

test('processGroup001', normal, compile_and_run, ['-package unix'])
test('processGroup002', normal, compile_and_run, ['-package unix'])
test('executeFile001', omit_ways(prof_ways), compile_and_run, ['-package unix'])

test('T8108', normal, compile_and_run, ['-package unix'])
