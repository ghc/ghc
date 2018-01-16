test('annotations', [extra_files(['AnnotationLet.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory annotations'])
test('parseTree',   [extra_files(['AnnotationTuple.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory parseTree'])
test('comments',    [extra_files(['CommentsTest.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory comments'])
test('exampleTest', [extra_files(['AnnotationTuple.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory exampleTest'])
test('listcomps',   [extra_files(['ListComprehensions.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory listcomps'])
test('T10255',      [extra_files(['Test10255.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10255'])
test('T10268',      [extra_files(['Test10268.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10268'])
test('T10269',      [extra_files(['Test10269.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10269'])
test('T10280',      [extra_files(['Test10280.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10280'])
test('T10312',      [extra_files(['Test10312.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10312'])
test('T10307',      [extra_files(['Test10307.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10307'])
test('T10309',      [extra_files(['Test10309.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10309'])
test('boolFormula', [extra_files(['TestBoolFormula.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory boolFormula'])
test('T10357',      [extra_files(['Test10357.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10357'])
test('T10358',      [extra_files(['Test10358.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10358'])
test('T10278',      [expect_broken(14529),
                     extra_files(['Test10278.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10278'])
test('T10354',      [extra_files(['Test10354.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10354'])
test('T10396',      [extra_files(['Test10396.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10396'])
test('T10399',      [expect_broken(14529),
                     extra_files(['Test10399.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10399'])
test('T10313',      [extra_files(['Test10313.hs', 'stringSource.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10313'])
test('T11018',      [extra_files(['Test11018.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T11018'])
test('bundle-export', [extra_files(['BundleExport.hs']),
                       ignore_stderr], run_command, ['$MAKE -s --no-print-directory bundle-export'])
test('T10276',      [extra_files(['Test10276.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10276'])
test('T10598',      [extra_files(['Test10598.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T10598'])
test('T11321',      [extra_files(['Test11321.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T11321'])
test('T11332',      [extra_files(['Test11332.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T11332'])
test('T11430',      [extra_files(['Test11430.hs', 't11430.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T11430'])
test('load-main',   ignore_stderr, run_command, ['$MAKE -s --no-print-directory load-main'])
test('T12417',      [extra_files(['Test12417.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T12417'])
test('T13163',      [extra_files(['Test13163.hs']),
                     ignore_stderr], run_command, ['$MAKE -s --no-print-directory T13163'])
