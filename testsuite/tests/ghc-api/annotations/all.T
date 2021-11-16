test('annotations', [extra_files(['AnnotationLet.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['annotations'])
test('parseTree',   [extra_files(['AnnotationTuple.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['parseTree'])
test('comments',    [extra_files(['CommentsTest.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['comments'])
test('exampleTest', [extra_files(['AnnotationTuple.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['exampleTest'])
test('listcomps',   [extra_files(['ListComprehensions.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['listcomps'])
test('T10255',      [extra_files(['Test10255.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10255'])
test('T10268',      [extra_files(['Test10268.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10268'])
test('T10269',      [extra_files(['Test10269.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10269'])
test('T10280',      [extra_files(['Test10280.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10280'])
test('T10312',      [extra_files(['Test10312.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10312'])
test('T10307',      [extra_files(['Test10307.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10307'])
test('T10309',      [extra_files(['Test10309.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10309'])
test('boolFormula', [extra_files(['TestBoolFormula.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['boolFormula'])
test('T10357',      [extra_files(['Test10357.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10357'])
test('T10358',      [extra_files(['Test10358.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10358'])
test('T10278',      [extra_files(['Test10278.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10278'])
test('T10354',      [extra_files(['Test10354.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10354'])
test('T10396',      [extra_files(['Test10396.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10396'])
test('T10399',      [extra_files(['Test10399.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10399'])
test('T10313',      [extra_files(['Test10313.hs', 'stringSource.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10313'])
test('T11018',      [extra_files(['Test11018.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T11018'])
test('bundle-export', [extra_files(['BundleExport.hs']),
                       javascript_failures,
                       ignore_stderr], makefile_test, ['bundle-export'])
test('T10276',      [extra_files(['Test10276.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10276'])
test('T10598',      [extra_files(['Test10598.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T10598'])
test('T11321',      [extra_files(['Test11321.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T11321'])
test('T11332',      [extra_files(['Test11332.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T11332'])
test('T11430',      [extra_files(['Test11430.hs', 't11430.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T11430'])
test('load-main',   [javascript_failures,
                     ignore_stderr], makefile_test, ['load-main'])
test('T12417',      [extra_files(['Test12417.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T12417'])
test('T13163',      [extra_files(['Test13163.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T13163'])
test('T15303',      [extra_files(['Test15303.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T15303'])
test('T16212',      [extra_files(['Test16212.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T16212'])
test('T16230',      [extra_files(['Test16230.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T16230'])
test('T16236',      [extra_files(['Test16236.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T16236'])
test('StarBinderAnns',      [extra_files(['StarBinderAnns.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['StarBinderAnns'])
test('T16279',      [extra_files(['Test16279.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T16279'])
test('T17388',      [extra_files(['Test17388.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T17388'])
test('T17519',      [extra_files(['Test17519.hs']),
                     javascript_failures,
                     ignore_stderr], makefile_test, ['T17519'])
