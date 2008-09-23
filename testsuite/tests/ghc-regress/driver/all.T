setTestOpts(compose(alone, only_compiler_types(['ghc'])))

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

test('driver011', normal, run_command,
     ['$MAKE -s --no-print-directory test011'])
cleanall()
test('driver012', normal, run_command,
     ['$MAKE -s --no-print-directory test012'])
cleanall()
test('driver013', normal, run_command,
     ['$MAKE -s --no-print-directory test013'])
cleanall()
test('driver014', extra_clean(['A014.hi', 'obj014/A014.o', 'obj014']),
     run_command,
     ['$MAKE -s --no-print-directory test014'])
test('driver015', extra_clean(['A015.o', 'hi015/A015.hi', 'hi015']),
     run_command,
     ['$MAKE -s --no-print-directory test015'])
test('driver016', normal, run_command,
     ['$MAKE -s --no-print-directory test016'])
cleanall()
test('driver017', normal, run_command,
     ['$MAKE -s --no-print-directory test017'])
cleanall()
test('driver018',
     extra_clean(['F018.hi', 'F018_stub.c', 'F018_stub.h',
                  'obj018/TestStub018.o', 'obj018/TestStub018_stub.o',
                  'obj018']),
     run_command,
     ['$MAKE -s --no-print-directory test018'])
test('driver019',
     extra_clean(['out019/TestStub.hi','out019/TestStub.o','out019/TestStub_stub.c','out019/TestStub_stub.h','out019/TestStub_stub.o']),
     run_command,
     ['$MAKE -s --no-print-directory test019'])
test('driver021', normal, run_command,
     ['$MAKE -s --no-print-directory test021'])
cleanall()
test('driver022', normal, run_command,
     ['$MAKE -s --no-print-directory test022'])
cleanall()
test('driver023', normal, run_command,
     ['$MAKE -s --no-print-directory test023'])
cleanall()
test('driver024',
     extra_clean(['B/C024.hi', 'obj024/B/C024.o', 'obj024/B', 'obj024']),
     run_command,
     ['$MAKE -s --no-print-directory test024'])
test('driver024a', normal, run_command,
     ['$MAKE -s --no-print-directory test024a'])
cleanall()
test('driver025', normal, run_command,
     ['$MAKE -s --no-print-directory test025'])
cleanall()
test('driver026', normal, run_command,
     ['$MAKE -s --no-print-directory test026'])
cleanall()
test('driver027', normal, run_command,
     ['$MAKE -s --no-print-directory test027'])
cleanall()
test('driver028', normal, run_command,
     ['$MAKE -s --no-print-directory test028'])
cleanall()
test('driver031', normal, run_command,
     ['$MAKE -s --no-print-directory test031'])
cleanall()
test('driver032', normal, run_command,
     ['$MAKE -s --no-print-directory test032'])
cleanall()
test('driver033', normal, run_command,
     ['$MAKE -s --no-print-directory test033'])
cleanall()
test('driver034', normal, run_command,
     ['$MAKE -s --no-print-directory test034'])
cleanall()
test('driver035', normal, run_command,
     ['$MAKE -s --no-print-directory test035'])
cleanall()
test('driver041', normal, run_command,
     ['$MAKE -s --no-print-directory test041'])
cleanall()
test('driver042', normal, run_command,
     ['$MAKE -s --no-print-directory test042'])
cleanall()
test('driver043', normal, run_command,
     ['$MAKE -s --no-print-directory test043'])
cleanall()
test('driver044', normal, run_command,
     ['$MAKE -s --no-print-directory test044'])
cleanall()
test('driver045', normal, run_command,
     ['$MAKE -s --no-print-directory test045'])
cleanall()
test('driver051', normal, run_command,
     ['$MAKE -s --no-print-directory test051'])
cleanall()
test('driver052', normal, run_command,
     ['$MAKE -s --no-print-directory test052'])
cleanall()
test('driver053', normal, run_command,
     ['$MAKE -s --no-print-directory test053'])
cleanall()

test('driver060', normal, run_command,
     ['$MAKE -s --no-print-directory test060'])
cleanall()
test('driver061', normal, run_command,
     ['$MAKE -s --no-print-directory test061'])
cleanall()
test('driver061a.1', normal, run_command,
     ['$MAKE -s --no-print-directory test061a.1'])
cleanall()
test('driver061a.2', normal, run_command,
     ['$MAKE -s --no-print-directory test061a.2'])
cleanall()

test('driver062.1', normal, run_command,
     ['$MAKE -s --no-print-directory test062.1'])
cleanall()
test('driver062.2', normal, run_command,
     ['$MAKE -s --no-print-directory test062.2'])
cleanall()
test('driver062.3', normal, run_command,
     ['$MAKE -s --no-print-directory test062.3'])
cleanall()
test('driver062.4', normal, run_command,
     ['$MAKE -s --no-print-directory test062.4'])
cleanall()
test('driver062.5', normal, run_command,
     ['$MAKE -s --no-print-directory test062.5'])
cleanall()

test('driver063', normal, run_command,
     ['$MAKE -s --no-print-directory test063'])
cleanall()
test('driver064', normal, run_command,
     ['$MAKE -s --no-print-directory test064'])
cleanall()
test('driver065', normal, run_command,
     ['$MAKE -s --no-print-directory test065'])
cleanall()
test('driver066', normal, run_command,
     ['$MAKE -s --no-print-directory test066'])
cleanall()
test('driver067', normal, run_command,
     ['$MAKE -s --no-print-directory test067'])
cleanall()
test('driver068', normal, run_command,
     ['$MAKE -s --no-print-directory test068'])
cleanall()
test('driver069', normal, run_command,
     ['$MAKE -s --no-print-directory test069'])
cleanall()
test('driver070', normal, run_command,
     ['$MAKE -s --no-print-directory test070'])
cleanall()
test('driver071', normal, run_command,
     ['$MAKE -s --no-print-directory test071'])
cleanall()

test('driver080', normal, run_command,
     ['$MAKE -s --no-print-directory test080'])
cleanall()

test('driver081.1', normal, run_command,
     ['$MAKE -s --no-print-directory test081.1'])
cleanall()
test('driver081.2', normal, run_command,
     ['$MAKE -s --no-print-directory test081.2'])
cleanall()

test('driver100',
     extra_clean(['overlap/List.hi', 'overlap/List.o',
                  'overlap/Overlap.hi', 'overlap/Overlap.o']),
     run_command,
     ['$MAKE -s --no-print-directory test100'])

test('driver200', normal, run_command,
     ['$MAKE -s --no-print-directory test200'])
cleanall()

test('2566', normal, run_command,
     ['$MAKE -s --no-print-directory 2566'])
cleanall()

test('pragma001', normal, compile, [''])
test('pragma002', normal, compile, [''])

test('2499', normal, compile_fail, [''])

test('mode001', normal, run_command,
     ['$MAKE -s --no-print-directory mode001'])

