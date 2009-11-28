setTestOpts(only_compiler_types(['ghc']))

def cleanall():
    clean(['A.hc', 'A.hi', 'A.hspp', 'A.o', 'A.ooo', 'A.s', 'A.tmp', 'A.xhi',
           'B/C.hi', 'B/C.o', 'B/C.ooo', 'B/C.xhi', 'B/stub.c',
           'B/F.hi', 'B/F.o', 'B/F_stub.c', 'B/F_stub.h', 'B/F_stub.o',
           'F.hi', 'F.o', 'F_stub.c', 'F_stub.h', 'F_stub.o',
           'Hello.hi', 'Hello.hs', 'Hello.o',
           'B/Hello.hi', 'B/Hello.hs', 'B/Hello.o',
           'd1/P/Q.hi', 'd1/P/Q.o', 'd2/R/S.hi', 'd2/R/S.o',
           'depend', 'depend.bak', 'hello'])

    if default_testopts.cleanup != '':
        rawSystem(['rm', '-rf', in_testdir('hi')])
        rawSystem(['rm', '-rf', in_testdir('obj')])
        rawSystem(['rm', '-rf', in_testdir('stub')])

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

test('driver021', alone, run_command,
     ['$MAKE -s --no-print-directory test021'])
cleanall()
test('driver022', alone, run_command,
     ['$MAKE -s --no-print-directory test022'])
cleanall()
test('driver023', alone, run_command,
     ['$MAKE -s --no-print-directory test023'])

test('driver024',
     extra_clean(['B024/C.hi', 'obj024/B024/C.o', 'obj024/B024', 'obj024']),
     run_command,
     ['$MAKE -s --no-print-directory test024'])

test('driver024a', alone, run_command,
     ['$MAKE -s --no-print-directory test024a'])
cleanall()
test('driver025', alone, run_command,
     ['$MAKE -s --no-print-directory test025'])
cleanall()
test('driver026', alone, run_command,
     ['$MAKE -s --no-print-directory test026'])
cleanall()
test('driver027', alone, run_command,
     ['$MAKE -s --no-print-directory test027'])
cleanall()
test('driver028', alone, run_command,
     ['$MAKE -s --no-print-directory test028'])
cleanall()
test('driver031', alone, run_command,
     ['$MAKE -s --no-print-directory test031'])
cleanall()
test('driver032', alone, run_command,
     ['$MAKE -s --no-print-directory test032'])
cleanall()
test('driver033', alone, run_command,
     ['$MAKE -s --no-print-directory test033'])
cleanall()
test('driver034', alone, run_command,
     ['$MAKE -s --no-print-directory test034'])
cleanall()
test('driver035', alone, run_command,
     ['$MAKE -s --no-print-directory test035'])
cleanall()
test('driver041', alone, run_command,
     ['$MAKE -s --no-print-directory test041'])
cleanall()
test('driver042', alone, run_command,
     ['$MAKE -s --no-print-directory test042'])
cleanall()
test('driver043', alone, run_command,
     ['$MAKE -s --no-print-directory test043'])
cleanall()
test('driver044', alone, run_command,
     ['$MAKE -s --no-print-directory test044'])
cleanall()
test('driver045', alone, run_command,
     ['$MAKE -s --no-print-directory test045'])
cleanall()
test('driver051', alone, run_command,
     ['$MAKE -s --no-print-directory test051'])
cleanall()
test('driver052', alone, run_command,
     ['$MAKE -s --no-print-directory test052'])
cleanall()
test('driver053', alone, run_command,
     ['$MAKE -s --no-print-directory test053'])
cleanall()

test('driver060', alone, run_command,
     ['$MAKE -s --no-print-directory test060'])
cleanall()
test('driver061', alone, run_command,
     ['$MAKE -s --no-print-directory test061'])
cleanall()
test('driver061a.1', alone, run_command,
     ['$MAKE -s --no-print-directory test061a.1'])
cleanall()
test('driver061a.2', alone, run_command,
     ['$MAKE -s --no-print-directory test061a.2'])
cleanall()

test('driver062.1', alone, run_command,
     ['$MAKE -s --no-print-directory test062.1'])
cleanall()
test('driver062.2', alone, run_command,
     ['$MAKE -s --no-print-directory test062.2'])
cleanall()
test('driver062.3', alone, run_command,
     ['$MAKE -s --no-print-directory test062.3'])
cleanall()
test('driver062.4', alone, run_command,
     ['$MAKE -s --no-print-directory test062.4'])
cleanall()
test('driver062.5', alone, run_command,
     ['$MAKE -s --no-print-directory test062.5'])
cleanall()

test('driver063', alone, run_command,
     ['$MAKE -s --no-print-directory test063'])
cleanall()
test('driver064', alone, run_command,
     ['$MAKE -s --no-print-directory test064'])
cleanall()
test('driver065', alone, run_command,
     ['$MAKE -s --no-print-directory test065'])
cleanall()
test('driver066', alone, run_command,
     ['$MAKE -s --no-print-directory test066'])
cleanall()
test('driver067', alone, run_command,
     ['$MAKE -s --no-print-directory test067'])
cleanall()
test('driver068', alone, run_command,
     ['$MAKE -s --no-print-directory test068'])
cleanall()
test('driver069', alone, run_command,
     ['$MAKE -s --no-print-directory test069'])
cleanall()
test('driver070', alone, run_command,
     ['$MAKE -s --no-print-directory test070'])
cleanall()
test('driver071', alone, run_command,
     ['$MAKE -s --no-print-directory test071'])
cleanall()

test('driver080', alone, run_command,
     ['$MAKE -s --no-print-directory test080'])
cleanall()

test('driver081.1', alone, run_command,
     ['$MAKE -s --no-print-directory test081.1'])
cleanall()
test('driver081.2', alone, run_command,
     ['$MAKE -s --no-print-directory test081.2'])
cleanall()

test('driver100',
     [alone,
      extra_clean(['overlap/List.hi', 'overlap/List.o',
                   'overlap/Overlap.hi', 'overlap/Overlap.o'])],
     run_command,
     ['$MAKE -s --no-print-directory test100'])

test('driver200', alone, run_command,
     ['$MAKE -s --no-print-directory test200'])
cleanall()

test('2566',
     [alone,
      normalise_fun(normalise_errmsg)],
     run_command,
     ['$MAKE -s --no-print-directory 2566'])
cleanall()

test('pragma001', alone, compile, [''])
test('pragma002', alone, compile, [''])

test('2499', alone, compile_fail, [''])

test('mode001', alone, run_command,
     ['$MAKE -s --no-print-directory mode001'])

if config.platform == 'i386-unknown-mingw32':
     only_windows = normal
else:
     only_windows = skip

test('shared001',
     [alone,
      only_windows,
      extra_clean(['Shared001.hi', 'Shared001.o', 'HSdll.dll.a', 'HSdll.dll', 'Shared001_stub.c', 'Shared001_stub.h', 'Shared001_stub.o'])],
     run_command,
     ['$MAKE -s --no-print-directory shared001'])

test('dynHelloWorld',
     [alone,
      only_ways(['dyn'])],
     compile_and_run,
     [''])

