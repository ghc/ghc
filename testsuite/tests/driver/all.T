test('driver011', [], run_command, ['$MAKE -s --no-print-directory test011'])

test('driver012', [], run_command, ['$MAKE -s --no-print-directory test012'])

test('driver013', [], run_command, ['$MAKE -s --no-print-directory test013'])

test('driver014', [], run_command, ['$MAKE -s --no-print-directory test014'])

test('driver015', [], run_command, ['$MAKE -s --no-print-directory test015'])

test('driver016', [], run_command, ['$MAKE -s --no-print-directory test016'])

test('driver017', [], run_command, ['$MAKE -s --no-print-directory test017'])

test('driver018', [], run_command, ['$MAKE -s --no-print-directory test018'])

test('driver018a', [], run_command, ['$MAKE -s --no-print-directory test018a'])

test('driver019', [], run_command, ['$MAKE -s --no-print-directory test019'])

test('driver021', [], run_command, ['$MAKE -s --no-print-directory test021'])

test('driver022', [], run_command, ['$MAKE -s --no-print-directory test022'])

test('driver023', [], run_command, ['$MAKE -s --no-print-directory test023'])

test('driver024', [], run_command, ['$MAKE -s --no-print-directory test024'])

test('driver024a', [], run_command, ['$MAKE -s --no-print-directory test024a'])

test('driver025', [], run_command, ['$MAKE -s --no-print-directory test025'])

test('driver026', [], run_command, ['$MAKE -s --no-print-directory test026'])

test('driver027', [], run_command, ['$MAKE -s --no-print-directory test027'])

test('driver028', [], run_command, ['$MAKE -s --no-print-directory test028'])

test('driver031', [], run_command, ['$MAKE -s --no-print-directory test031'])

test('driver032', [], run_command, ['$MAKE -s --no-print-directory test032'])

test('driver033', [], run_command, ['$MAKE -s --no-print-directory test033'])

test('driver034', [], run_command, ['$MAKE -s --no-print-directory test034'])

test('driver035', [], run_command, ['$MAKE -s --no-print-directory test035'])

test('driver041', [], run_command, ['$MAKE -s --no-print-directory test041'])

test('driver042', [], run_command, ['$MAKE -s --no-print-directory test042'])

test('driver042stub', [], run_command,
     ['$MAKE -s --no-print-directory test042stub'])

test('driver043', [], run_command, ['$MAKE -s --no-print-directory test043'])

test('driver044', [], run_command, ['$MAKE -s --no-print-directory test044'])

test('driver045', [], run_command, ['$MAKE -s --no-print-directory test045'])

test('driver051', [], run_command, ['$MAKE -s --no-print-directory test051'])

test('driver052', [], run_command, ['$MAKE -s --no-print-directory test052'])

test('driver053', [], run_command, ['$MAKE -s --no-print-directory test053'])

test('driver061a', [], run_command, ['$MAKE -s --no-print-directory test061a'])

test('driver061b', [], run_command, ['$MAKE -s --no-print-directory test061b'])


test('driver062a', [], run_command, ['$MAKE -s --no-print-directory test062a'])
test('driver062b', [], run_command, ['$MAKE -s --no-print-directory test062b'])
test('driver062c', [], run_command, ['$MAKE -s --no-print-directory test062c'])
test('driver062d', [], run_command, ['$MAKE -s --no-print-directory test062d'])
test('driver062e', [], run_command, ['$MAKE -s --no-print-directory test062e'])

test('driver063', [], run_command, ['$MAKE -s --no-print-directory test063'])

test('driver064', [], run_command, ['$MAKE -s --no-print-directory test064'])

test('driver065', [], run_command, ['$MAKE -s --no-print-directory test065'])

test('driver066', [], run_command, ['$MAKE -s --no-print-directory test066'])

test('driver067', [], run_command, ['$MAKE -s --no-print-directory test067'])

test('driver070', [], run_command, ['$MAKE -s --no-print-directory test070'])

test('driver071', [], run_command, ['$MAKE -s --no-print-directory test071'])

test('driver081a', [], run_command, ['$MAKE -s --no-print-directory test081a'])

test('driver081b', [], run_command, ['$MAKE -s --no-print-directory test081b'])

test('driver100', [], run_command, ['$MAKE -s --no-print-directory test100'])

test('driver200', [], run_command, ['$MAKE -s --no-print-directory test200'])

test('T2566',
     normalise_fun(normalise_errmsg),
     run_command,
     ['$MAKE -s --no-print-directory T2566'])

test('pragma001', normal, compile, [''])
test('pragma002', normal, compile, [''])

test('T2499', normal, compile_fail, [''])

test('mode001', normal, run_command,
     ['$MAKE -s --no-print-directory mode001'])

if config.os == 'mingw32':
     only_windows = normal
else:
     only_windows = skip

test('shared001', [only_windows], run_command,
     ['$MAKE -s --no-print-directory shared001'])

if config.os == 'darwin':
  only_darwin = normal
else:
  only_darwin = skip

test('static001', [only_darwin, expect_broken(8127)], run_command,
     ['$MAKE -s --no-print-directory static001'])

test('dynHelloWorld',
     only_ways(['dyn']),
     compile_and_run,
     [''])

test('T5313',
     [ when(arch('powerpc64') or arch('powerpc64le'), expect_broken(11259)),
       extra_run_opts('"' + config.libdir + '"')],
     compile_and_run,
     ['-package ghc'])

test('T2464', normal, compile, [''])
test('T3674', [], run_command, ['$MAKE -s --no-print-directory T3674'])

test('rtsopts001', [], run_command,
     ['$MAKE -s --no-print-directory rtsopts001'])
test('rtsopts002', normal, run_command,
                ['$MAKE -s --no-print-directory rtsopts002'])

test('withRtsOpts', [], run_command,
     ['$MAKE -s --no-print-directory withRtsOpts'])
test('T3389', normal, compile_and_run, [''])
test('T3364', normalise_fun(normalise_errmsg), run_command,
     ['$MAKE -s --no-print-directory T3364'])

# T4437: A test to ensure that Cabal's language extension list remains
# consistent with those known by GHC.
#
# See Note [Adding a language extension] in DynFlags.hs for details.
test('T4437', only_ways(['normal']), compile_and_run, ['-package ghc'])

test('werror', normal, compile_fail, [''])

test('T706', [req_interp], run_command, ['$MAKE -s --no-print-directory T706'])
test('spacesInArgs',
     extra_run_opts('"a   b"   "c   d"'),
     compile_and_run,
     [''])

fobject_code = when(doing_ghci(), extra_hc_opts('-fobject-code'))

test( 'T4114a', fobject_code, compile_and_run, ['-cpp'])
test( 'T4114b', fobject_code, compile_and_run, ['-no-keep-hi-files'])
test( 'T4114c', fobject_code, compile_and_run, ['-no-keep-o-files'])
test( 'T4114d', fobject_code, compile_and_run,
     ['-hisuf .myhi -osuf .myo -no-keep-o-files'])

test('T5584', [], run_command, ['$MAKE -s --no-print-directory T5584'])
test('T5198', [], run_command, ['$MAKE -s --no-print-directory T5198'])
test('T7060', [], run_command, ['$MAKE -s --no-print-directory T7060'])
test('T7130', normal, compile_fail, ['-fflul-laziness'])
test('T7563', when(unregisterised(), skip), run_command,
     ['$MAKE -s --no-print-directory T7563'])
test('T6037',
     # The testsuite doesn't know how to set a non-Unicode locale on Windows or Mac OS X
     [when(opsys('mingw32'), expect_fail), when(opsys('darwin'), expect_fail)],
     run_command,
     ['$MAKE -s --no-print-directory T6037'])
test('T2507',
     # The testsuite doesn't know how to set a non-Unicode locale on Windows or Mac OS X
     [when(opsys('mingw32'), expect_fail), when(opsys('darwin'), expect_fail)],
     run_command,
     ['$MAKE -s --no-print-directory T2507'])
test('T8959a',
     # The testsuite doesn't know how to set a non-Unicode locale on Windows or Mac OS X
     [when(opsys('mingw32'), expect_fail), when(opsys('darwin'), expect_fail)],
     run_command,
     ['$MAKE -s --no-print-directory T8959a'])

test('T703', normal, run_command, ['$MAKE -s --no-print-directory T703'])
test('T2182', normal, run_command, ['$MAKE -s --no-print-directory T2182'])
test('T8101', expect_broken(10600), compile, ['-Wall -fno-code'])
test('T8101b', expect_broken(10600), multimod_compile,
     ['T8101b', '-Wall -fno-code'])

# Should not panic when compiling cmm file together with -outputdir.
test('T9050', cmm_src, compile, ['-outputdir=.'])

test('write_interface_oneshot', [], run_command,
     ['$MAKE -s --no-print-directory write_interface_oneshot'])

test('write_interface_make', [], run_command,
     ['$MAKE -s --no-print-directory write_interface_make'])

test('T9776', normal, compile_fail, ['-frule-check'])

test('T9938', [], run_command, ['$MAKE -s --no-print-directory T9938'])

test('T9938B', [], run_command, ['$MAKE -s --no-print-directory T9938B'])

test('T9963', exit_code(1), run_command,
     ['{compiler} --interactive -ignore-dot-ghci --print-libdir'])

test('T10219', normal, run_command,
     # `-x hspp` in make mode should work.
     # Note: need to specify `-x hspp` before the filename.
     ['{compiler} --make -x hspp T10219.hspp -fno-code -v0'])

test('T10220', normal, run_command,
     # Preprocessed T10220.hspp imports T10220B. Should work in --make mode.
     ['{compiler} --make T10220.hspp -fno-code -v0'])

test('T10182', [], run_command, ['$MAKE -s --no-print-directory T10182'])

test('T365',
     [pre_cmd('touch test_preprocessor.txt'), unless(opsys('mingw32'), skip)],
     compile_fail, [''])

test('T9360a', normal, run_command, ['{compiler} --interactive -e "" -ignore-dot-ghci'])
test('T9360b', normal, run_command, ['{compiler} -e "" --interactive -ignore-dot-ghci'])

test('T10970', normal, compile_and_run, [''])
test('T4931', normal, compile_and_run, [''])
test('T11182', normal, compile_and_run, [''])
test('T11381', normal, compile_fail, [''])
test('T11429a', normal, compile, ['-Wunrecognised-warning-flags -Wfoobar'])
test('T11429b', normal, compile, ['-Wno-unrecognised-warning-flags -Wfoobar'])
test('T11429c', normal, compile_fail, ['-Wunrecognised-warning-flags -Werror -Wfoobar'])
test('T11763', normal, compile_and_run, ['-fno-version-macros'])

test('T10320', [], run_command, ['$MAKE -s --no-print-directory T10320'])

test('T12135', [expect_broken(12135)], run_command,
     ['$MAKE -s --no-print-directory T12135'])

test('T12192', normal, run_command, ['mkdir foo && (cd foo && {compiler} -v0 ../T12192)'])

test('T10923', [], run_command, ['$MAKE -s --no-print-directory T10923'])

test('T12752pass', normal, compile, ['-DSHOULD_PASS=1 -Wcpp-undef'])

test('T12955', normal, run_command, ['$MAKE -s --no-print-directory T12955'])

test('T12971', ignore_stdout, run_command, ['$MAKE -s --no-print-directory T12971'])
test('json', normal, compile_fail, ['-ddump-json'])
test('json2', normal, compile, ['-ddump-types -ddump-json'])
