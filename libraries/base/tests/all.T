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
test('SystemInfoTest', normal, compile_and_run, [''])
test('readFloat', exit_code(1), compile_and_run, [''])
test('read-float-double', normal, compile_and_run, [''])
test('enumDouble', normal, compile_and_run, [''])
test('enumRatio', normal, compile_and_run, [''])
test('enumNumeric', normal, compile_and_run, [''])
# N.B. the tempfile format is slightly different than this test expects on
# Windows *except* if using WinIO. The `when` clause below can be removed
# after WinIO becomes the default.
test('tempfiles', when(opsys('mingw32'), only_ways(['winio'])), compile_and_run, [''])
test('fixed', normal, compile_and_run, [''])
test('quotOverflow', normal, compile_and_run, [''])
test('assert', exit_code(1), compile_and_run, ['-fno-ignore-asserts'])
test('CPUTime001', fragile(16224), compile_and_run, [''])
test('readLitChar',   normal, compile_and_run, [''])
test('unicode001',
     normal,
     compile_and_run,
     [''])
test('unicode002',
     normal,
     compile_and_run,
     [''])
test('unicode003', normal, compile_and_run, [''])
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
test('isValidNatural', normal, compile_and_run, [''])

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
     expect_fail_for(['normal', 'threaded1', 'llvm', 'nonmoving', 'nonmoving_thr', 'nonmoving_thr_ghc']),
     # JS doesn't support stack limit so the test sometimes passes just fine. Therefore the test is
     # marked as fragile.
     when(js_arch(), fragile(22921))],
     compile_and_run, [''])

test('ratio001', 	normal, compile_and_run, [''])

test('reads001',		normal, compile_and_run, [''])
test('show001',		normal, compile_and_run, [''])
test('text001',		normal, compile_and_run, [''])

test('tup001',		normal, compile_and_run, [''])

test('addr001',		normal, compile_and_run, [''])
test('dynamic001',      normal, compile_and_run, [''])
test('dynamic002',      normal, compile_and_run, [''])
test('dynamic003',      extra_run_opts('+RTS -K32m -RTS'), compile_and_run, [''])
test('dynamic004',      omit_ways(['normal', 'threaded1']), compile_and_run, [''])
test('dynamic005',      normal, compile_and_run, [''])

enum_setups = [when(fast(), skip)]
def enum_test(name, omit):
    """
    These tests have a funky Python preprocessor which require some headstands
    to run on Windows.
    """
    if opsys('mingw32'):
        test(name,
             [when(opsys('mingw32'), extra_files(['enum_processor.bat'])),
              omit_ways(omit),
              extra_files(['enum_processor.py'])],
             compile_and_run,
             ['-F -pgmF ./enum_processor.bat'])
    else:
        test(name,
             [extra_files(['enum_processor.py']),
              omit_ways(omit)],
             compile_and_run,
             ['-F -pgmF ./enum_processor.py'])

enum_test('enum01', ['ghci-opt'])
enum_test('enum02', [])
enum_test('enum03', [])
test('enum04', normal, compile_and_run, [''])

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
test('T4006', [normalise_fun(normalise_quotes), req_process], compile_and_run, [''])

test('T5943', normal, compile_and_run, [''])
test('T5962', normal, compile_and_run, [''])
test('T7034', normal, compile_and_run, [''])

test('qsem001', normal, compile_and_run, [''])
test('qsemn001', normal, compile_and_run, [''])

test('T7457', normal, compile_and_run, [''])

test('T7773',
     [when(opsys('mingw32'), skip),
      js_broken(22261),
      when(arch('wasm32'), fragile(24928)),
      expect_broken_for(23272, ['ghci-opt']) # unclear
     ],
     compile_and_run,
     [''])
# Andreas says that T7773 will not (and should not) work on Windows

# Tests for kind-polymorphic Category
test('CatPairs', normal, compile, [''])
test('CatEntail', normal, compile, [''])

# When running with WAY=ghci and profiled ways, T7653 uses a lot of memory.
test('T7653', [when(opsys('mingw32'), skip),
               when(arch('wasm32'), run_timeout_multiplier(5)),
               omit_ways(prof_ways + ghci_ways)], compile_and_run, [''])
test('T7787', normal, compile_and_run, [''])

test('topHandler01', [when(opsys('mingw32'), skip), js_broken(22261), req_process], compile_and_run, [''])
test('topHandler02',
     [when(opsys('mingw32'), skip),
      omit_ghci,
      signal_exit_code(2),
     js_broken(22261)
     ], compile_and_run, [''])
test('topHandler03',
     [when(opsys('mingw32'), skip), ignore_stderr,
      signal_exit_code(15),
     js_broken(22261)
     ], compile_and_run, [''])
test('topHandler04',
     [when(opsys('mingw32'), skip),
      exit_code(1)
     ], compile_and_run, [''])


test('T8766',
        [ collect_stats('bytes allocated',5)
        , only_ways(['normal'])
        , js_broken(22261)
        ],
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
#
# We ignore stderr since the test itself may print "Killed: 9" (see #24361);
# all we care about is that the test timed out, for which the
# exit_code check is sufficient.
test('T8089',
     [exit_code(99), ignore_stderr, run_timeout_multiplier(0.01)],
     compile_and_run, [''])
test('T8684', expect_broken(8684), compile_and_run, [''])
test('hWaitForInput-accurate-stdin', [js_broken(22349), expect_broken_for(16535, threaded_ways), req_process], compile_and_run, [''])
test('T9826',normal, compile_and_run,[''])
test('T9848',
        [ collect_stats('bytes allocated')
        , only_ways(['normal'])
        , js_broken(22261)
        ],
      compile_and_run,
      ['-O'])
test('T10149', normal, compile_and_run, [''])
test('T11334a', normal, compile_and_run, [''])
test('T11555', normal, compile_and_run, [''])
test('T12494', normal, compile_and_run, [''])
test('T12852', [when(opsys('mingw32'), skip), js_broken(22374), req_process], compile_and_run, [''])
test('lazySTexamples', normal, compile_and_run, [''])
test('T11760', [req_ghc_smp,
               req_target_smp,
               only_ways(['threaded1', 'threaded2', 'nonmoving_thr'])],
               compile_and_run, ['-threaded -with-rtsopts=-N2'])
test('T12874', normal, compile_and_run, [''])
test('T13191',
        [ collect_stats('bytes allocated', 5)
        , only_ways(['normal'])
        , js_broken(22261)
        ],
      compile_and_run,
      ['-O'])
test('T13525', [when(opsys('mingw32'), skip), js_broken(22374), req_process], compile_and_run, [''])
test('T13097', normal, compile_and_run, [''])
test('T13660', when(opsys('mingw32'), skip), compile_and_run, [''])
test('functorOperators', normal, compile_and_run, [''])
test('T3474',
     [collect_stats('max_bytes_used',5),
      js_broken(22374),
      only_ways(['normal'])],
     compile_and_run, ['-O'])
test('T14425', normal, compile_and_run, [''])
test('T10412', normal, compile_and_run, [''])
test('T13896', normal, compile_and_run, [''])
# On Windows this test is fragile using the old MIO IO manager due to an
# apparent flushing bug.
test('T13167',
     [ when(opsys('mingw32')
     , only_ways(['winio', 'winio_threaded']))
     , fragile_for(16536, concurrent_ways)
       # fragile on JS
     , when(js_arch(), fragile(22921))
     ],
     compile_and_run, [''])
test('T15183', normal, compile_and_run, [''])
test('T15349', [exit_code(1), expect_broken_for(15349, ghci_ways)], compile_and_run, [''])
test('T16111', exit_code(1), compile_and_run, [''])
test('T16943a', normal, compile_and_run, [''])
test('T16943b', normal, compile_and_run, [''])
test('T17499', [collect_stats('bytes allocated',5), js_broken(22261)], compile_and_run, ['-O -w'])
test('T16643', normal, compile_and_run, [''])
test('clamp', normal, compile_and_run, [''])
test('T18642',
     [extra_run_opts('+RTS -T -RTS'),
      # The nonmoving GC's residency behavior is very conservative
      omit_ways(['nonmoving', 'nonmoving_thr', 'nonmoving_thr_sanity'] + ghci_ways),
      js_broken(22374)],
     compile_and_run, ['-O2'])
test('T19288', exit_code(1), compile_and_run, [''])
test('T19719', normal, compile_and_run, [''])
test('T20107', extra_run_opts('+RTS -M50M'), compile_and_run, ['-package bytestring'])
test('T22816', normal, compile_and_run, [''])
test('trace', normal, compile_and_run, [''])
test('listThreads', normal, compile_and_run, [''])
test('listThreads1', omit_ghci, compile_and_run, [''])
test('inits1tails1', normal, compile_and_run, [''])
test('CLC149', normal, compile, [''])
test('AtomicModifyIORef', js_fragile(24259), compile_and_run, [''])
test('AtomicSwapIORef', normal, compile_and_run, [''])
test('T23454', normal, compile_fail, [''])
test('T23687', normal, compile_and_run, [''])
test('T23697',
  [ when(opsys('mingw32'), skip) # header not found
  , when(opsys('darwin'), skip)  # permission denied
  ], makefile_test, ['T23697'])
test('stimesEndo', normal, compile_and_run, [''])
test('T24807', exit_code(1), compile_and_run, [''])
test('T25066', [only_ways(['optasm']), grep_errmsg('T25066.g')], compile, ['-ddump-dmd-signatures'])
