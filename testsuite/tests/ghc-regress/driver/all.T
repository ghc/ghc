setTestOpts(only_compiler_types(['ghc']))

test('driver011',
     extra_clean(['A011.hi', 'A011.o']),
     run_command,
     ['$MAKE -s --no-print-directory test011'])

test('driver012',
     extra_clean(['A012.hi', 'A012.o']),
     run_command,
     ['$MAKE -s --no-print-directory test012'])

test('driver013',
     extra_clean(['A013.hi', 'A013.o']),
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
     extra_clean(['F016.hi', 'F016.o', 'F016_stub.c', 'F016_stub.h']),
     run_command,
     ['$MAKE -s --no-print-directory test016'])

test('driver017',
     extra_clean(['F017.hi', 'F017.o', 'F017_stub.c', 'F017_stub.h']),
     run_command,
     ['$MAKE -s --no-print-directory test017'])

test('driver018',
     extra_clean(['F018.hi', 'F018_stub.c', 'F018_stub.h',
                  'obj018/TestStub018.o', 'obj018/TestStub018_stub.o',
                  'obj018']),
     run_command,
     ['$MAKE -s --no-print-directory test018'])

test('driver018a',
     extra_clean(['F018a.hi', 'F018a_stub.c', 'F018a_stub.h',
                  'TestStub018a.obj.018',
                  'obj018a/TestStub018a_stub.obj.018']),
     run_command,
     ['$MAKE -s --no-print-directory test018a'])

test('driver019',
     extra_clean(['out019/TestStub.hi',
                  'out019/TestStub.o',
                  'out019/TestStub_stub.c',
                  'out019/TestStub_stub.h',
                  'out019/TestStub_stub.o']),
     run_command,
     ['$MAKE -s --no-print-directory test019'])

test('driver021',
     extra_clean(['B021/C.hi', 'B021/C.o']),
     run_command,
     ['$MAKE -s --no-print-directory test021'])

test('driver022',
     extra_clean(['B022/C.hi', 'B022/C.o']),
     run_command,
     ['$MAKE -s --no-print-directory test022'])

test('driver023',
     extra_clean(['B023/C.hi', 'B023/C.o']),
     run_command,
     ['$MAKE -s --no-print-directory test023'])

test('driver024',
     extra_clean(['B024/C.hi', 'obj024/B024/C.o', 'obj024/B024', 'obj024']),
     run_command,
     ['$MAKE -s --no-print-directory test024'])

test('driver024a',
     extra_clean(['B024a/stub.c',
                  'obj024a/B024a/stub.o', 'obj024a/B024a', 'obj024a']),
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
                  'B027/F_stub.c', 'B027/F_stub.h', 'B027/F_stub.o']),
     run_command,
     ['$MAKE -s --no-print-directory test027'])

test('driver028',
     extra_clean(['B028/F.hi', 'B028/F.o',
                  'B028/F_stub.c', 'B028/F_stub.h', 'B028/F_stub.o']),
     run_command,
     ['$MAKE -s --no-print-directory test028'])

test('driver031',
     extra_clean(['A031.hi', 'A031.o']),
     run_command,
     ['$MAKE -s --no-print-directory test031'])

test('driver032',
     extra_clean(['A032.hi', 'A032.o']),
     run_command,
     ['$MAKE -s --no-print-directory test032'])

test('driver033',
     extra_clean(['A033.hi', 'A033.o', 'hi033/A033.hi', 'hi033']),
     run_command,
     ['$MAKE -s --no-print-directory test033'])

test('driver034',
     extra_clean(['F034.hi', 'F034.o', 'F034_stub.c', 'F034_stub.h']),
     run_command,
     ['$MAKE -s --no-print-directory test034'])

test('driver035',
     extra_clean(['F035.hi', 'F035.o',
                  'stub035/F035_stub.c', 'stub035/F035_stub.h']),
     run_command,
     ['$MAKE -s --no-print-directory test035'])

test('driver041',
     extra_clean(['B041/C.hi', 'B041/C.o']),
     run_command,
     ['$MAKE -s --no-print-directory test041'])

test('driver042',
     extra_clean(['B042/C.hi', 'B042/C.o']),
     run_command,
     ['$MAKE -s --no-print-directory test042'])

test('driver043',
     extra_clean(['B043/C.hi', 'B043/C.o',
                  'hi043/B043/C.hi', 'hi043/B043', 'hi043']),
     run_command,
     ['$MAKE -s --no-print-directory test043'])

test('driver044',
     extra_clean(['B044/F.hi', 'B044/F.o',
                  'B044/F_stub.c', 'B044/F_stub.h', 'B044/F_stub.o']),
     run_command,
     ['$MAKE -s --no-print-directory test044'])

test('driver045',
     extra_clean(['B045/F.hi', 'B045/F.o',
                  'B045/F_stub.c', 'B045/F_stub.h', 'B045/F_stub.o']),
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

test('driver060',
     extra_clean(['A060.hi', 'A060.o', 'A060.hc']),
     run_command,
     ['$MAKE -s --no-print-directory test060'])

test('driver061',
     extra_clean(['A061.hi', 'A061.o', 'A061.hc']),
     run_command,
     ['$MAKE -s --no-print-directory test061'])

test('driver061a',
     extra_clean(['A061a.hi', 'A061a.o', 'A061a.s']),
     run_command,
     ['$MAKE -s --no-print-directory test061a'])

test('driver061b',
     extra_clean(['A061b.hi', 'A061b.o', 'A061b.s']),
     run_command,
     ['$MAKE -s --no-print-directory test061b'])


test('driver062a',
     extra_clean(['Hello062a.hs']),
     run_command,
     ['$MAKE -s --no-print-directory test062a'])
test('driver062b',
     extra_clean(['Hello062b.hs']),
     run_command,
     ['$MAKE -s --no-print-directory test062b'])
test('driver062c',
     extra_clean(['Hello062c.hs']),
     run_command,
     ['$MAKE -s --no-print-directory test062c'])
test('driver062d',
     extra_clean(['B062d/Hello062d.hs']),
     run_command,
     ['$MAKE -s --no-print-directory test062d'])
test('driver062e',
     extra_clean(['B062e/Hello062e.hs']),
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

test('driver068',
     extra_clean(['A068.hi', 'A068.o', 'A068.hc']),
     run_command,
     ['$MAKE -s --no-print-directory test068'])

test('driver069',
     extra_clean(['A069.hi', 'A069.o', 'A069.tmp']),
     run_command,
     ['$MAKE -s --no-print-directory test069'])

test('driver070',
     extra_clean(['A070.hi', 'A070.o', 'A070.s']),
     run_command,
     ['$MAKE -s --no-print-directory test070'])

test('driver071',
     extra_clean(['A071.hi', 'A071.o', 'A071.tmp']),
     run_command,
     ['$MAKE -s --no-print-directory test071'])

test('driver080',
     extra_clean(['A080.hi', 'A080.o', 'A080.hc']),
     run_command,
     ['$MAKE -s --no-print-directory test080'])

test('driver081a',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory test081a'])

test('driver081b',
     normal,
     run_command,
     ['$MAKE -s --no-print-directory test081b'])

test('driver100',
     extra_clean(['overlap/List.hi', 'overlap/List.o',
                  'overlap/Overlap.hi', 'overlap/Overlap.o']),
     run_command,
     ['$MAKE -s --no-print-directory test100'])

test('driver200',
     extra_clean(['depend200']),
     run_command,
     ['$MAKE -s --no-print-directory test200'])

test('2566',
     normalise_fun(normalise_errmsg),
     run_command,
     ['$MAKE -s --no-print-directory 2566'])

test('pragma001', normal, compile, [''])
test('pragma002', normal, compile, [''])

test('2499', normal, compile_fail, [''])

test('mode001', normal, run_command,
     ['$MAKE -s --no-print-directory mode001'])

if config.platform == 'i386-unknown-mingw32':
     only_windows = normal
else:
     only_windows = skip

test('shared001',
     [only_windows,
      extra_clean(['Shared001.hi', 'Shared001.o', 'HSdll.dll.a', 'HSdll.dll', 'Shared001_stub.c', 'Shared001_stub.h', 'Shared001_stub.o'])],
     run_command,
     ['$MAKE -s --no-print-directory shared001'])

test('dynHelloWorld',
     only_ways(['dyn']),
     compile_and_run,
     [''])

test('2464', normal, compile, [''])
test('3674', ignore_output, run_command, 
             ['$MAKE -s --no-print-directory 3674'])

test('rtsOpts', normal, run_command,
                ['$MAKE -s --no-print-directory rtsOpts'])

test('withRtsOpts', normal, run_command,
                    ['$MAKE -s --no-print-directory withRtsOpts'])
test('T3389', expect_broken(3389), compile_and_run, [''])

