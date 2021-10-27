test('driver011', [extra_files(['A011.hs'])], makefile_test, ['test011'])

test('driver012', [extra_files(['A012.hs'])], makefile_test, ['test012'])

test('driver013', [extra_files(['A013.hs'])], makefile_test, ['test013'])

test('driver014', [extra_files(['A014.hs'])], makefile_test, ['test014'])

test('driver015', [extra_files(['A015.hs'])], makefile_test, ['test015'])

test('driver016', [extra_files(['F016.hs'])], makefile_test, ['test016'])

test('driver017', [extra_files(['F017.hs'])], makefile_test, ['test017'])

test('driver018', [extra_files(['F018.hs'])], makefile_test, ['test018'])

test('driver018a', [extra_files(['F018a.hs'])], makefile_test, ['test018a'])

test('driver019', [extra_files(['F019.hs'])], makefile_test, ['test019'])

test('driver021', [extra_files(['B021/'])], makefile_test, ['test021'])

test('driver022', [extra_files(['B022/'])], makefile_test, ['test022'])

test('driver023', [extra_files(['B023/'])], makefile_test, ['test023'])

test('driver024', [extra_files(['B024/'])], makefile_test, ['test024'])

test('driver024a', [], makefile_test, ['test024a'])

test('driver025', [extra_files(['B025/'])], makefile_test, ['test025'])

test('driver026', [extra_files(['d026/'])], makefile_test, ['test026'])

test('driver027', [extra_files(['B027/'])], makefile_test, ['test027'])

test('driver028', [extra_files(['B028/'])], makefile_test, ['test028'])

test('driver031', [extra_files(['A031.hs'])], makefile_test, ['test031'])

test('driver032', [extra_files(['A032.hs'])], makefile_test, ['test032'])

test('driver033', [extra_files(['A033.hs'])], makefile_test, ['test033'])

test('driver034', [extra_files(['F034.hs'])], makefile_test, ['test034'])

test('driver035', [extra_files(['F035.hs'])], makefile_test, ['test035'])

test('driver041', [extra_files(['B041/'])], makefile_test, ['test041'])

test('driver042', [extra_files(['B042/'])], makefile_test, ['test042'])

test('driver042stub', [extra_files(['B042stub/'])], makefile_test, ['test042stub'])

test('driver043', [extra_files(['B043/'])], makefile_test, ['test043'])

test('driver044', [extra_files(['B044/'])], makefile_test, ['test044'])

test('driver045', [extra_files(['B045/'])], makefile_test, ['test045'])

test('driver051', [extra_files(['d051_1/', 'd051_2/'])], makefile_test, ['test051'])

test('driver052', [extra_files(['d052_1/', 'd052_2/'])], makefile_test, ['test052'])

test('driver053', [extra_files(['d053_1/', 'd053_2/'])], makefile_test, ['test053'])

test('driver061a', [extra_files(['A061a.hs'])], makefile_test, ['test061a'])

test('driver061b', [extra_files(['A061b.hs'])], makefile_test, ['test061b'])


test('driver062a', [], makefile_test, ['test062a'])
test('driver062b', [], makefile_test, ['test062b'])
test('driver062c', [], makefile_test, ['test062c'])
test('driver062d', [], makefile_test, ['test062d'])
test('driver062e', [], makefile_test, ['test062e'])

test('driver063', [extra_files(['D063.hs'])], makefile_test, ['test063'])

test('driver064', [extra_files(['A064.hs'])], makefile_test, ['test064'])

test('driver065', [extra_files(['A065.hs'])], makefile_test, ['test065'])

test('driver066', [extra_files(['A066.hs'])], makefile_test, ['test066'])

test('driver067', [extra_files(['A067.hs'])], makefile_test, ['test067'])

test('driver070', [extra_files(['A070.hs'])], makefile_test, ['test070'])

test('driver071', [extra_files(['A071.hs'])], makefile_test, ['test071'])

test('driver081a', [], makefile_test, ['test081a'])

test('driver081b', [], makefile_test, ['test081b'])

test('driver100', [extra_files(['overlap/'])], makefile_test, ['test100'])

test('driver200', [extra_files(['A200.hs', 'B200/', 'D200.hs'])], makefile_test, ['test200'])

test('T2566', normalise_fun(normalise_errmsg), makefile_test, [])

test('pragma001', normal, compile, [''])
test('pragma002', normal, compile, [''])

test('T2499', normal, compile_fail, [''])

test('mode001', normal, makefile_test, [])

if config.os == 'mingw32':
     only_windows = normal
else:
     only_windows = skip

test('shared001', [extra_files(['Shared001.hs']),
                   only_windows],
     makefile_test, ['shared001'])

if config.os == 'darwin':
  only_darwin = normal
else:
  only_darwin = skip

test('static001', [extra_files(['Static001.hs']),
                   only_darwin,
                   when(arch('x86_64'), expect_broken(8127))],
     makefile_test, ['static001'])

test('dynHelloWorld',
     only_ways(['dyn']),
     compile_and_run,
     [''])

test('T5313',
     [ req_rts_linker, extra_run_opts('"' + config.libdir + '"')],
     compile_and_run,
     ['-package ghc'])

test('T2464', normal, compile, [''])
test('T3674', [], makefile_test, [])

test('rtsopts001', [extra_files(['rtsOpts.hs'])], makefile_test, [])
test('rtsopts002', normal, makefile_test, [])

test('withRtsOpts', [], makefile_test, [])

test('T3389', when(arch('i386'), expect_broken_for(17256, ['hpc'] + prof_ways)), compile_and_run, [''])
test('T3364', normalise_fun(normalise_errmsg), makefile_test, [])

# T4437: A test to ensure that Cabal's language extension list remains
# consistent with those known by GHC.
#
# See Note [Adding a language extension] in GHC.Driver.Session for details.
test('T4437', only_ways(['normal']), compile_and_run, ['-package ghc'])

test('werror', normal, compile_fail, [''])

test('T706', [req_interp], makefile_test, [])
test('spacesInArgs',
     extra_run_opts('"a   b"   "c   d"'),
     compile_and_run,
     [''])

fobject_code = when(doing_ghci(), extra_hc_opts('-fobject-code'))

test( 'T4114a', fobject_code, compile_and_run, ['-cpp'])
test( 'T4114b', fobject_code, compile_and_run, ['-no-keep-hi-files'])
test( 'T4114c', [fobject_code, expect_broken_for(4114, ['ghci'])], compile_and_run, ['-no-keep-o-files'])
test( 'T4114d', [fobject_code, expect_broken_for(4114, ['ghci'])], compile_and_run,
     ['-hisuf .myhi -osuf .myo -no-keep-o-files'])

test('T5584', [], makefile_test, [])
test('T5198', [], makefile_test, [])
test('T7060', [], makefile_test, [])
# N.B. The typo in the argument below is intentional.
test('T7130', normal, compile_fail, ['-fflul-laziness'])
test('T7563', when(unregisterised(), skip), makefile_test, [])
test('T6037',
     # The testsuite doesn't know how to set a non-Unicode locale on Windows or Mac OS X
     [when(opsys('mingw32'), expect_fail), when(opsys('darwin'), expect_fail)],
     makefile_test, [])
test('T2507',
     # The testsuite doesn't know how to set a non-Unicode locale on Windows or Mac OS X
     [when(opsys('mingw32'), expect_fail), when(opsys('darwin'), expect_fail)],
     makefile_test, [])
test('T8959a',
     # The testsuite doesn't know how to set a non-Unicode locale on Windows or Mac OS X
     [when(opsys('mingw32'), expect_fail), when(opsys('darwin'), expect_fail)],
     makefile_test, [])

# Requires readelf
test('T703', unless(opsys('linux'), skip), makefile_test, [])
test('T2182', normal, makefile_test, [])
test('T8101', normal, compile, ['-Wall -fno-code'])
test('T8101b', normal, multimod_compile,
     ['T8101b', '-Wall -fno-code'])
test('T10600', normal, compile_fail, ['-fno-code'])

# Should not panic when compiling cmm file together with -outputdir.
test('T9050', cmm_src, compile, ['-outputdir=. -no-hs-main'])

test('write_interface_oneshot', [extra_files(['A011.hs'])], makefile_test, [])

test('write_interface_make', [extra_files(['A011.hs'])], makefile_test, [])

test('T9776', normal, compile_fail, ['-drule-check'])

test('T9938', [], makefile_test, [])

test('T9938B', [], makefile_test, [])

test('T9963', exit_code(1), run_command,
     ['{compiler} --interactive -ignore-dot-ghci --print-libdir'])

test('T10219', normal, run_command,
     # `-x hspp` in make mode should work.
     # Note: need to specify `-x hspp` before the filename.
     ['{compiler} --make -x hspp T10219.hspp -fno-code -v0'])

test('T10220', normal, run_command,
     # Preprocessed T10220.hspp imports T10220B. Should work in --make mode.
     ['{compiler} --make T10220.hspp -fno-code -v0'])

test('T10182', [], makefile_test, [])

test('T10869', [], makefile_test, [])

test('T365',
     [pre_cmd('touch test_preprocessor.txt'), unless(opsys('mingw32'), skip)],
     compile_fail, [''])

test('T9360a', req_interp, run_command, ['{compiler} --interactive -e "" -ignore-dot-ghci'])
test('T9360b', req_interp, run_command, ['{compiler} -e "" --interactive -ignore-dot-ghci'])

test('T10970', normal, compile_and_run, [''])
test('T4931', normal, compile_and_run, [''])
test('T11182', normal, compile_and_run, [''])
test('T11381', normal, compile_fail, [''])
test('StringListOptions', normal, compile, [''])
test('T11429a', normal, compile, ['-Wunrecognised-warning-flags -Wfoobar'])
test('T11429b', normal, compile, ['-Wno-unrecognised-warning-flags -Wfoobar'])
test('T11429c', normal, compile_fail, ['-Wunrecognised-warning-flags -Werror -Wfoobar'])
test('T11763', normal, compile_and_run, ['-fno-version-macros'])

test('T10320', [], makefile_test, [])

test('T12056a', normal, compile, ['-w -Wfoo -Wbar'])
test('T12056b', normal, compile, ['-w -XOverlappingInstances -Wfoo -Wunrecognised-warning-flags -Wbar'])
test('T12056c', normal, compile,
     ['-w -Wdeprecated-flags -XOverlappingInstances -Wfoo -Wunrecognised-warning-flags -Wbar'])

test('T12135', [expect_broken(12135)], makefile_test, [])

test('T12192', normal, run_command, ['mkdir foo && (cd foo && {compiler} -v0 ../T12192)'])

test('T10923', [], makefile_test, [])

test('T12625', normal, compile_fail, ['-I'])

test('T12752pass', normal, compile, ['-DSHOULD_PASS=1 -Wcpp-undef'])

test('T12955', normal, makefile_test, [])

test('T12971', [when(opsys('mingw32'), fragile(17945)), ignore_stdout], makefile_test, [])
test('json', normal, compile_fail, ['-ddump-json'])

# json2 test is sensitive to the LLVM not supported ouput from GHC. ANd the error
# won't tell.  It looks unrelated and is annoying to debug. Hence we disable the
# warning to prevent spurious errors.
test('json2', normalise_version('base','ghc-prim'), compile, ['-ddump-types -ddump-json -Wno-unsupported-llvm-version'])
test('T16167', exit_code(1), run_command,
     ['{compiler} -x hs -e ":set prog T16167.hs" -ddump-json T16167.hs'])
test('T13604', [], makefile_test, [])
test('T13604a', [], makefile_test, [])
# omitting hpc and profasm because they affect the
# inlining and unfoldings
test('inline-check', omit_ways(['hpc', 'profasm'])
                   , compile
		   , ['-dinline-check foo -O -ddebug-output'])

test('T14452', [], makefile_test, [])
test('T14923', normal, makefile_test, [])
test('T15396', normal, compile_and_run, ['-package ghc'])
test('T16737',
     [extra_files(['T16737include/']),
      req_th,
      expect_broken_for(16541, ['ghci'])],
     compile_and_run, ['-optP=-isystem -optP=T16737include'])

test('T17143', exit_code(1), run_command, ['{compiler} T17143.hs -S -fno-code'])
test('T17786', unless(opsys('mingw32'), skip), makefile_test, [])
test('T18369', normal, compile, ['-O'])
test('FullGHCVersion', normal, compile_and_run, ['-package ghc-boot'])
test('OneShotTH', normal, makefile_test, [])
test('T17481', normal, makefile_test, [])
test('T20084', normal, makefile_test, [])
test('RunMode', extra_files(['RunMode/Test.hs']), run_command, ['{compiler} --run -iRunMode/ -ignore-dot-ghci RunMode.hs -- hello'])
test('T20439', normal, run_command,
     ['echo "module Foo where" >> Foo.hs && \
      {compiler} -E -fno-code -XCPP -v Foo.hs 2>&1 | grep "Copying" | sed "s/.*to//" '])
test('T20459', normal, multimod_compile_fail,
     ['T20459B', ''])
test('T20200loop', extra_files(['T20200loop']), multimod_compile,
     ['Datatypes', '-iT20200loop -O -v0'])
