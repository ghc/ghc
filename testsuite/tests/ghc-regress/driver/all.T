setTestOpts(only_compiler_types(['ghc']))

test('driver011', normal, run_command_ignore_output, ['$MAKE test011'])
test('driver012', normal, run_command_ignore_output, ['$MAKE test012'])
test('driver013', normal, run_command_ignore_output, ['$MAKE test013'])
test('driver014', normal, run_command_ignore_output, ['$MAKE test014'])
test('driver015', normal, run_command_ignore_output, ['$MAKE test015'])
test('driver021', normal, run_command_ignore_output, ['$MAKE test021'])
test('driver022', normal, run_command_ignore_output, ['$MAKE test022'])
test('driver023', normal, run_command_ignore_output, ['$MAKE test023'])
test('driver024', normal, run_command_ignore_output, ['$MAKE test024'])
test('driver025', normal, run_command_ignore_output, ['$MAKE test025'])
test('driver026', normal, run_command_ignore_output, ['$MAKE test026'])
test('driver031', normal, run_command_ignore_output, ['$MAKE test031'])
test('driver032', normal, run_command_ignore_output, ['$MAKE test032'])
test('driver033', normal, run_command_ignore_output, ['$MAKE test033'])
test('driver041', normal, run_command_ignore_output, ['$MAKE test041'])
test('driver042', normal, run_command_ignore_output, ['$MAKE test042'])
test('driver043', normal, run_command_ignore_output, ['$MAKE test043'])
test('driver051', normal, run_command_ignore_output, ['$MAKE test051'])
test('driver052', normal, run_command_ignore_output, ['$MAKE test052'])
test('driver053', normal, run_command_ignore_output, ['$MAKE test053'])

test('driver060', normal, run_command_ignore_output, ['$MAKE test060'])
test('driver061', normal, run_command_ignore_output, ['$MAKE test061'])
test('driver062', normal, run_command_ignore_output, ['$MAKE test062'])
test('driver063', normal, run_command, ['$MAKE -s --no-print-directory test063'])
test('driver064', normal, run_command_ignore_output, ['$MAKE test064'])
test('driver065', normal, run_command_ignore_output, ['$MAKE test065'])
test('driver066', normal, run_command_ignore_output, ['$MAKE test066'])
test('driver067', normal, run_command_ignore_output, ['$MAKE test067'])
test('driver068', normal, run_command_ignore_output, ['$MAKE test068'])
test('driver069', normal, run_command_ignore_output, ['$MAKE test069'])
test('driver070', normal, run_command_ignore_output, ['$MAKE test070'])
test('driver071', normal, run_command_ignore_output, ['$MAKE test071'])

test('driver080', normal, run_command_ignore_output, ['$MAKE test080'])

test('driver200', normal, run_command_ignore_output, ['$MAKE test200'])

clean(['B/C.hi', 'B/C.o', 'B/C.ooo', 'B/C.xhi',
       'd1/P/Q.o', 'd2/R/S.o', 'd1/P/Q.hi', 'd2/R/S.hi',
       'A.ooo', 'A.xhi', 'Hello.hi', 'Hello.o', 'hello', 'depend'])

if default_testopts.cleanup != '':
    os.spawnlp(os.P_WAIT, 'rm', 'rm', '-rf', in_testdir('obj'))
    os.spawnlp(os.P_WAIT, 'rm', 'rm', '-rf', in_testdir('hi'))

