test('comments',    [extra_files(['CommentsTest.hs']),
                     ignore_stderr], makefile_test, ['comments'])
test('listcomps',   [extra_files(['ListComprehensions.hs']),
                     normalise_slashes,
                     ignore_stderr], makefile_test, ['listcomps'])
test('InTreeAnnotations1',normal, compile, ['-dsuppress-uniques -ddump-parsed-ast'])
