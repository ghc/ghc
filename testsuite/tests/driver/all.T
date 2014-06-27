setTestOpts(only_compiler_types(['ghc']))

test('driver011',
     extra_clean(['A011.hi', 'A011.o']),
     run_command,
     ['$MAKE -s --no-print-directory test011'])

test('driver012',
     extra_clean(['A012.hi', 'A012.ooo']),
     run_command,
     ['$MAKE -s --no-print-directory test012'])

test('driver013',
     extra_clean(['A013.xhi', 'A013.o']),
     run_command,
     ['$MAKE -s --no-print-directory test013'])

test('driver014',
     extra_clean(['A014.hi', 'obj014/A014.o', 'obj014']),
     run_command,
     ['$MAKE -s --no-print-directory test014'])

test('driver015',
     extra_clean(['A015.o', 'hi015/A015.hi', 'hi015']),
     run_command,
     ['$MAKE -s --no-print-directory test015'])

test('driver016',
     extra_clean(['F016.hi', 'F016.o', 'F016_stub.h']),
     run_command,
     ['$MAKE -s --no-print-directory test016'])

test('driver017',
     extra_clean(['F017.hi', 'F017.o',
                  'stub017/TestStub017_stub.h', 'stub017/']),
     run_command,
     ['$MAKE -s --no-print-directory test017'])

test('driver018',
     extra_clean(['F018.hi', 'F018_stub.h',
                  'obj018/TestStub018.o', 'obj018/TestStub018_stub.o',
                  'obj018']),
     run_command,
     ['$MAKE -s --no-print-directory test018'])

test('driver018a',
     extra_clean(['F018a.hi', 'F018a_stub.h',
                  'F018a.obj.018']),
     run_command,
     ['$MAKE -s --no-print-directory test018a'])

test('driver019',
     extra_clean(['out019/TestStub019.hi',
                  'out019/TestStub019.o',
                  'out019/TestStub019_stub.h',
                  'out019/TestStub019_stub.o']),
     run_command,
     ['$MAKE -s --no-print-directory test019'])

test('driver021',
     extra_clean(['B021/C.hi', 'B021/C.o']),
     run_command,
     ['$MAKE -s --no-print-directory test021'])

test('driver022',
     extra_clean(['B022/C.hi', 'B022/C.ooo']),
     run_command,
     ['$MAKE -s --no-print-directory test022'])

test('driver023',
     extra_clean(['B023/C.xhi', 'B023/C.o']),
     run_command,
     ['$MAKE -s --no-print-directory test023'])

test('driver024',
     extra_clean(['B024/C.hi', 'obj024/B024/C.o', 'obj024/B024', 'obj024']),
     run_command,
     ['$MAKE -s --no-print-directory test024'])

test('driver024a',
     extra_clean(['obj024a/B024a/stub.o', 'obj024a/B024a', 'obj024a',
                  'B024a/stub.c', 'B024a/']),
     run_command,
     ['$MAKE -s --no-print-directory test024a'])

test('driver025',
     extra_clean(['B025/C.hi', 'B025/C.o',
                  'hi025/B025/C.hi', 'hi025/B025', 'hi025']),
     run_command,
     ['$MAKE -s --no-print-directory test025'])

test('driver026',
     extra_clean(['d026/P/Q.hi', 'd026/P/Q.o']),
     run_command,
     ['$MAKE -s --no-print-directory test026'])

test('driver027',
     extra_clean(['B027/F.hi', 'B027/F.o',
                  'B027/F_stub.h', 'B027/F_stub.o']),
     run_command,
     ['$MAKE -s --no-print-directory test027'])

test('driver028',
     extra_clean(['B028/F.hi', 'B028/F.o',
                  'stub028/B028/F_stub.h', 'stub028/B028/', 'stub028/']),
     run_command,
     ['$MAKE -s --no-print-directory test028'])

test('driver031',
     extra_clean(['A031.hi', 'A031.o']),
     run_command,
     ['$MAKE -s --no-print-directory test031'])

test('driver032',
     extra_clean(['A032.hi', 'obj032/A032.o', 'obj032/']),
     run_command,
     ['$MAKE -s --no-print-directory test032'])

test('driver033',
     extra_clean(['A033.hi', 'A033.o', 'hi033/A033.hi', 'hi033']),
     run_command,
     ['$MAKE -s --no-print-directory test033'])

test('driver034',
     extra_clean(['F034.hi', 'F034.o', 'F034_stub.h']),
     run_command,
     ['$MAKE -s --no-print-directory test034'])

test('driver035',
     extra_clean(['F035.hi', 'F035.o',
                  'stub035/TestStub035_stub.h', 'stub035/']),
     run_command,
     ['$MAKE -s --no-print-directory test035'])

test('driver041',
     extra_clean(['B041/C.hi', 'B041/C.o']),
     run_command,
     ['$MAKE -s --no-print-directory test041'])

test('driver042',
     extra_clean(['B042/C.hi', 'obj042/B042/C.o', 'obj042/B042/', 'obj042/']),
     run_command,
     ['$MAKE -s --no-print-directory test042'])

test('driver042stub',
     extra_clean(['B042stub/C.hi', 'obj042stub/B042stub/C.o', 'obj042stub/B042stub/', 'obj042stub/']),
     run_command,
     ['$MAKE -s --no-print-directory test042stub'])

test('driver043',
     extra_clean(['B043/C.hi', 'B043/C.o',
                  'hi043/B043/C.hi', 'hi043/B043', 'hi043']),
     run_command,
     ['$MAKE -s --no-print-directory test043'])

test('driver044',
     extra_clean(['B044/F.hi', 'B044/F.o',
                  'B044/F_stub.h', 'B044/F_stub.o']),
     run_command,
     ['$MAKE -s --no-print-directory test044'])

test('driver045',
     extra_clean(['B045/F.hi', 'B045/F.o',
                  'stub045/B045/F_stub.h', 'stub045/B045/', 'stub045/']),
     run_command,
     ['$MAKE -s --no-print-directory test045'])

test('driver051',
     extra_clean(['d051_1/P/Q.hi', 'd051_1/P/Q.o',
                  'd051_2/R/S.hi', 'd051_2/R/S.o']),
     run_command,
     ['$MAKE -s --no-print-directory test051'])

test('driver052',
     extra_clean(['d052_1/P/Q.hi', 'd052_1/P/Q.o',
                  'd052_2/R/S.hi', 'd052_2/R/S.o',
                  'obj052/P/Q.o', 'obj052/P',
                  'obj052/R/S.o', 'obj052/R',
                  'obj052']),
     run_command,
     ['$MAKE -s --no-print-directory test052'])

test('driver053',
     extra_clean(['d053_1/P/Q.hi', 'd053_1/P/Q.o',
                  'd053_2/R/S.hi', 'd053_2/R/S.o',
                  'hi053/P/Q.hi', 'hi053/P',
                  'hi053/R/S.hi', 'hi053/R',
                  'hi053']),
     run_command,
     ['$MAKE -s --no-print-directory test053'])

test('driver061a',
     extra_clean(['A061a.hi', 'A061a.o', 'A061a.s']),
     run_command,
     ['$MAKE -s --no-print-directory test061a'])

test('driver061b',
     extra_clean(['A061b.hi', 'A061b.o', 'A061b.s']),
     run_command,
     ['$MAKE -s --no-print-directory test061b'])


test('driver062a',
     extra_clean(['Hello062a.hs', 'Hello062a.hi', 'Hello062a.o',
                  'hello062a', 'hello062a.exe']),
     run_command,
     ['$MAKE -s --no-print-directory test062a'])
test('driver062b',
     extra_clean(['Hello062b.hs', 'Hello062b.hi', 'Hello062b.o',
                  'hello062b', 'hello062b.exe']),
     run_command,
     ['$MAKE -s --no-print-directory test062b'])
test('driver062c',
     extra_clean(['Hello062c.hs', 'Hello062c.hi', 'Hello062c.o',
                  'hello062c', 'hello062c.exe']),
     run_command,
     ['$MAKE -s --no-print-directory test062c'])
test('driver062d',
     extra_clean(['B062d/Hello062d.hi', 'B062d/Hello062d.o',
                  'B062d/Hello062d.hs', 'B062d/',
                  'hello062d', 'hello062d.exe']),
     run_command,
     ['$MAKE -s --no-print-directory test062d'])
test('driver062e',
     extra_clean(['B062e/Hello062e.hi', 'B062e/Hello062e.o',
                  'B062e/Hello062e.hs', 'B062e/',
                  'hello062e', 'hello062e.exe']),
     run_command,
     ['$MAKE -s --no-print-directory test062e'])

test('driver063',
     extra_clean(['A063.hi', 'A063.o', 'A063.hc',
                  'D063.hi', 'D063.o', 'D063.hc']),
     run_command,
     ['$MAKE -s --no-print-directory test063'])

test('driver064',
     extra_clean(['A064.hi', 'A064.o', 'A064.hspp']),
     run_command,
     ['$MAKE -s --no-print-directory test064'])

test('driver065',
     extra_clean(['A065.hi', 'A065.o', 'A065.hspp']),
     run_command,
     ['$MAKE -s --no-print-directory test065'])

test('driver066',
     extra_clean(['A066.hi', 'A066.o', 'A066.tmp']),
     run_command,
     ['$MAKE -s --no-print-directory test066'])

test('driver067',
     extra_clean(['A067.hi', 'A067.o', 'A067.tmp']),
     run_command,
     ['$MAKE -s --no-print-directory test067'])

test('driver070',
     extra_clean(['A070.hi', 'A070.o', 'A070.s']),
     run_command,
     ['$MAKE -s --no-print-directory test070'])

test('driver071',
     extra_clean(['A071.hi', 'A071.o', 'A071.tmp']),
     run_command,
     ['$MAKE -s --no-print-directory test071'])

test('driver081a',
     extra_clean(['Test_081a.hs', 'Test_081a.hi', 'Test_081a.o',
                  'Test_081a', 'Test_081a.exe']),
     run_command,
     ['$MAKE -s --no-print-directory test081a'])

test('driver081b',
     extra_clean(['Test.081b.hs', 'Test.081b.hi', 'Test.081b.o',
                  'Test.081b', 'Test.081b.exe']),
     run_command,
     ['$MAKE -s --no-print-directory test081b'])

test('driver100',
     extra_clean(['overlap/List.hi', 'overlap/List.o',
                  'overlap/Overlap.hi', 'overlap/Overlap.o']),
     run_command,
     ['$MAKE -s --no-print-directory test100'])

test('driver200',
     extra_clean(['depend200', 'depend200.bak']),
     run_command,
     ['$MAKE -s --no-print-directory test200'])

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

test('shared001',
     [only_windows,
      extra_clean(['Shared001.hi', 'Shared001.o', 'HSdll.dll.a', 'HSdll.dll', 'Shared001_stub.h', 'Shared001_stub.o'])],
     run_command,
     ['$MAKE -s --no-print-directory shared001'])

if config.os == 'darwin':
  only_darwin = normal
else:
  only_darwin = skip

test('static001',
     [only_darwin, expect_fail,
      extra_clean(['Static001.hi', 'Static001.o', 'liba.a', 'Static001_stub.h', 'Static001_stub.o'])],
     run_command,
     ['$MAKE -s --no-print-directory static001'])

test('dynHelloWorld',
     only_ways(['dyn']),
     compile_and_run,
     [''])

test('T5313',
     extra_run_opts('"' + config.libdir + '"'),
     compile_and_run,
     ['-package ghc'])

test('T2464', normal, compile, [''])
test('T3674',
     [ignore_output,
      extra_clean(['T3674_pre.hi', 'T3674_pre.o', 'T3674_pre', 'T3674_pre.exe'])],
     run_command, 
     ['$MAKE -s --no-print-directory T3674'])

test('rtsopts001',
     extra_clean(['rtsOpts.hi', 'rtsOpts.o', 'rtsOpts', 'rtsOpts.exe']),
     run_command,
     ['$MAKE -s --no-print-directory rtsopts001'])
test('rtsopts002', normal, run_command,
                ['$MAKE -s --no-print-directory rtsopts002'])

test('withRtsOpts', [ extra_clean(['withRtsOpts', 'withRtsOpts.exe',
                                   'withRtsOpts.hi', 'withRtsOpts.o',
                                   'withRtsOpts.out']) ],
                    run_command,
                    ['$MAKE -s --no-print-directory withRtsOpts'])
test('T3389', normal, compile_and_run, [''])
test('T3364', normalise_fun(normalise_errmsg), run_command,
     ['$MAKE -s --no-print-directory T3364'])
test('T4437', only_ways(['normal']), compile_and_run, ['-package ghc'])

test('werror', normal, compile_fail, [''])

test('T706',
     [req_interp,
      extra_clean(['T706.hs'])],
     run_command,
     ['$MAKE -s --no-print-directory T706'])
test('spacesInArgs',
     extra_run_opts('"a   b"   "c   d"'),
     compile_and_run,
     [''])

test('T5584',
     extra_clean(['T5584_in/A.hi-boot', 'T5584_out/T5584/A.o-boot']),
     run_command, ['$MAKE -s --no-print-directory T5584'])
test('T5198', extra_clean(['T5198dump/T5198.dump-simpl', 'T5198dump']),
     run_command,
     ['$MAKE -s --no-print-directory T5198'])
test('T7060',
     extra_clean(['T7060dump/T7060.dump-rules',
                  'T7060dump/T7060.dump-rule-firings',
                  'T7060dump/T7060.dump-rule-rewrites',
                  'T7060dump']),
     run_command,
     ['$MAKE -s --no-print-directory T7060'])
test('T7130', normal, compile_fail, ['-fflul-laziness'])
test('T7563', when(unregisterised(), skip), run_command,
     ['$MAKE -s --no-print-directory T7563'])
test('T6037', expect_broken(6037), run_command,
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
test('T8101', normal, compile, ['-Wall -fno-code'])

def build_T9050(name, way):
     return simple_build(name + '.cmm', way, '-outputdir=. ', 0, '', 0, 0, 0)
test('T9050', normal, build_T9050, [])

test('write_interface_oneshot', normal, run_command, ['$MAKE -s --no-print-directory write_interface_oneshot'])
test('write_interface_make', normal, run_command, ['$MAKE -s --no-print-directory write_interface_make'])
test('no_write_interface', normal, run_command, ['$MAKE -s --no-print-directory no_write_interface'])
