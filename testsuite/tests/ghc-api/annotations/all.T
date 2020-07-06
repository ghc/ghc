test('comments',    [extra_files(['CommentsTest.hs']),
                     ignore_stderr], makefile_test, ['comments'])
test('exampleTest', [extra_files(['AnnotationTuple.hs']),
                     ignore_stderr], makefile_test, ['exampleTest'])
test('listcomps',   [extra_files(['ListComprehensions.hs']),
                     normalise_slashes,
                     ignore_stderr], makefile_test, ['listcomps'])
test('bundle-export', [extra_files(['BundleExport.hs']),
                       ignore_stderr], makefile_test, ['bundle-export'])
test('load-main',   ignore_stderr, makefile_test, ['load-main'])

test('InTreeAnnotations1',normal, compile, ['-dsuppress-uniques -ddump-parsed-ast'])
