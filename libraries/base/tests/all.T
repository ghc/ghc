import string
import re

#--------------------------------------
# Python normalization functions
#--------------------------------------

def normalise_quotes (str):
  str = re.sub(r'"',r'', str, flags=re.MULTILINE)
  return str

#--------------------------------------
# Test functions
#--------------------------------------
test('readFloat', exit_code(1), compile_and_run, [''])
test('enumDouble', normal, compile_and_run, [''])
test('enumRatio', normal, compile_and_run, [''])
test('enumNumeric', normal, compile_and_run, [''])
test('tempfiles', normal, compile_and_run, [''])
test('fixed', normal, compile_and_run, [''])
test('quotOverflow', normal, compile_and_run, [''])
test('assert', exit_code(1), compile_and_run, ['-fno-ignore-asserts'])
test('CPUTime001', normal, compile_and_run, [''])
test('readLitChar',   normal, compile_and_run, [''])
test('unicode001',
     when(platform('i386-unknown-openbsd'), expect_fail),
     compile_and_run,
     [''])
test('unicode002',
     when(platform('i386-unknown-openbsd'), expect_fail),
     compile_and_run,
     [''])
test('data-fixed-show-read', normal, compile_and_run, [''])
test('showDouble', normal, compile_and_run, [''])
test('readDouble001', normal, compile_and_run, [''])
test('readInteger001', normal, compile_and_run, [''])
test('readFixed001', normal, compile_and_run, [''])
test('lex001', normal, compile_and_run, [''])
test('take001', extra_run_opts('1'), compile_and_run, [''])
test('inits', normal, compile_and_run, [''])
test('genericNegative001', extra_run_opts('-1'), compile_and_run, [''])
test('ix001', normal, compile_and_run, [''])

# need to add -K64m to the compiler opts, so that GHCi gets it too
test('ioref001',
     [when(fast(), skip),extra_run_opts('+RTS -K64m -RTS')],
     compile_and_run,
     ['+RTS -K64m -RTS'])

test('echo001', set_stdin("echo001.hs"), compile_and_run, [''])

test('hTell001', normal, compile_and_run, [''])

test('hTell002', normal, compile_and_run, [''])

test('performGC001', normal, compile_and_run, [''])

# optimisation screws up this test because some of the traces get commoned up
test('trace001', normal, compile_and_run, [''])

test('hGetBuf002', normal, compile_and_run, [''])
test('hGetBuf003', normal, compile_and_run, [''])
test('hPutBuf001', normal, compile_and_run, [''])
test('hPutBuf002', [], compile_and_run, [''])

test('char001', 	normal, compile_and_run, [''])
test('char002', 	normal, compile_and_run, [''])

test('cstring001', 	normal, compile_and_run, [''])

test('length001',
     # length001 depends on a RULE being applied, and without -O takes
     # excessive amounts of stack space. So we specifically set a low
     # stack limit and mark it as failing under a few conditions.
     [extra_run_opts('+RTS -K8m -RTS'),
     expect_fail_for(['normal', 'threaded1', 'llvm'])],
     compile_and_run, [''])

test('ratio001', 	normal, compile_and_run, [''])

test('rand001',		reqlib('random'), compile_and_run, [''])
test('reads001',		normal, compile_and_run, [''])
test('show001',		normal, compile_and_run, [''])
test('text001',		normal, compile_and_run, [''])

test('tup001',		normal, compile_and_run, [''])

test('addr001',		normal, compile_and_run, [''])
test('dynamic001',      normal, compile_and_run, [''])
test('dynamic002',      normal, compile_and_run, [''])
test('dynamic003',      extra_run_opts('+RTS -K32m -RTS'), compile_and_run, [''])
test('dynamic004',      omit_ways(['normal', 'threaded1', 'ghci']), compile_and_run, [''])
test('dynamic005',      normal, compile_and_run, [''])

enum_setups = [when(fast(), skip)]
test('enum01',          [extra_files(['enum_processor.bat', 'enum_processor.py']),
                         enum_setups], compile_and_run, [''])
test('enum02',          [extra_files(['enum_processor.bat', 'enum_processor.py']),
                         enum_setups], compile_and_run, [''])
test('enum03',          [extra_files(['enum_processor.bat', 'enum_processor.py']),
                         enum_setups], compile_and_run, [''])
test('enum04',          normal, compile_and_run, [''])

test('exceptionsrun001',        normal, compile_and_run, [''])
test('exceptionsrun002', 	normal, compile_and_run, [''])
test('foldableArray',   normal, compile_and_run, [''])
test('list001'	,	when(fast(), skip), compile_and_run, [''])
test('list002', when(fast(), skip), compile_and_run, [''])
test('list003', when(fast(), skip), compile_and_run, [''])
test('isSuffixOf', normal, compile_and_run, [''])

test('memo001', [extra_files(['Memo1.lhs']),
                 extra_run_opts('+RTS -A10k -RTS')], multimod_compile_and_run,
     ['memo001', ''])

test('memo002', [extra_files(['Memo2.lhs']),
                 extra_run_opts('20')], multimod_compile_and_run,
     ['memo002', ''])

test('stableptr001',
     [when(fast(), skip), extra_run_opts('+RTS -K8m -RTS')],
     compile_and_run, [''])
test('stableptr003', normal, compile_and_run, [''])
test('stableptr004', extra_run_opts('+RTS -K4m -RTS'), compile_and_run, [''])
test('stableptr005', normal, compile_and_run, [''])

test('weak001', normal, compile_and_run, [''])

test('T2528', normal, compile_and_run, [''])

# In the 65001 codepage, we can't even cat the expected output on msys:
#     $ cat 4006.stdout
#     It works here
#     cat: write error: Permission denied
# Seems to be a known problem, e.g.
#     http://mingw-users.1079350.n2.nabble.com/Bug-re-Unicode-on-the-console-td3121717.html
# May 2014: seems to work on msys2
# May 2018: The behavior of printf seems very implementation dependent.
#     so let's normalise the output.
test('T4006', normalise_fun(normalise_quotes), compile_and_run, [''])

test('T5943', normal, compile_and_run, [''])
test('T5962', normal, compile_and_run, [''])
test('T7034', normal, compile_and_run, [''])

test('qsem001', normal, compile_and_run, [''])
test('qsemn001', normal, compile_and_run, [''])

test('T7457', normal, compile_and_run, [''])

test('T7773', when(opsys('mingw32'), skip), compile_and_run, [''])
# Andreas says that T7773 will not (and should not) work on Windows

# Tests for kind-polymorphic Category
test('CatPairs', normal, compile, [''])
test('CatEntail', normal, compile, [''])

# When running with WAY=ghci, T7653 uses a lot of memory.
test('T7653', high_memory_usage, compile_and_run, [''])
test('T7787', normal, compile_and_run, [''])

test('topHandler01', when(opsys('mingw32'), skip), compile_and_run, [''])
test('topHandler02',
     [when(opsys('mingw32'), skip),
      omit_ways(['ghci']),
      signal_exit_code(2)
     ], compile_and_run, [''])
test('topHandler03',
     [when(opsys('mingw32'), skip), ignore_stderr,
      signal_exit_code(15)
     ], compile_and_run, [''])
test('topHandler04',
     [when(opsys('mingw32'), skip),
      exit_code(1)
     ], compile_and_run, [''])


test('T8766',
        [ collect_stats('bytes allocated',5)
        , only_ways(['normal'])],
      compile_and_run,
      ['-O'])

test('T9111', normal, compile, [''])
test('T9395', normal, compile_and_run, [''])
# Fails for debug way due to annotation linting timeout
test('T9532', omit_ways(['debug']), compile_and_run, [''])
test('T9586', normal, compile, [''])
test('T9681', normal, compile_fail, [''])
# Test no runtime crash. Report success and kill with `timeout` (exit code 99)
# after a few seconds. From https://phabricator.haskell.org/D1075:
#
#   "I used a fairly conservative timeout. IF there is a regression it will
#   crash as soon as the timeout's C call is done. The tricky bit is
#   guesstimating how much time it needs to run to guarantee it's reached the
#   C call.
#   Probably something like 1s is already enough, but I don't know enough to
#   make an educated guess how long it needs to be guaranteed to reach the C
#   call."
test('T8089',
     [exit_code(99), run_timeout_multiplier(0.01)],
     compile_and_run, [''])
test('hWaitForInput-accurate-socket', reqlib('unix'), compile_and_run, [''])
test('T8684', expect_broken(8684), compile_and_run, [''])
test('hWaitForInput-accurate-stdin', normal, compile_and_run, [''])
test('hWaitForInput-accurate-pipe', reqlib('unix'), compile_and_run, [''])
test('T9826',normal, compile_and_run,[''])
test('T9848',
        [ collect_stats('bytes allocated')
        , only_ways(['normal'])],
      compile_and_run,
      ['-O'])
test('T10149', normal, compile_and_run, [''])
test('T11334a', normal, compile_and_run, [''])
test('T11555', normal, compile_and_run, [''])
test('T12494', normal, compile_and_run, [''])
test('T12852', when(opsys('mingw32'), skip), compile_and_run, [''])
test('lazySTexamples', normal, compile_and_run, [''])
test('T11760', req_smp, compile_and_run, ['-threaded -with-rtsopts=-N2'])
test('T12874', normal, compile_and_run, [''])
test('T13191',
        [ collect_stats('bytes allocated', 5)
        , only_ways(['normal'])],
      compile_and_run,
      ['-O'])
test('T13525', when(opsys('mingw32'), skip), compile_and_run, [''])
test('T13097', normal, compile_and_run, [''])
test('functorOperators', normal, compile_and_run, [''])
test('T3474',
     [collect_stats('max_bytes_used',5),
      only_ways(['normal'])],
     compile_and_run, ['-O'])
test('T14425', normal, compile_and_run, [''])
test('T10412', normal, compile_and_run, [''])
test('T13896', normal, compile_and_run, [''])
test('T13167', normal, compile_and_run, [''])
test('T15349', [exit_code(1), expect_broken_for(15349, 'ghci')], compile_and_run, [''])
