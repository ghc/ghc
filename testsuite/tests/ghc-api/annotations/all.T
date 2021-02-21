test('comments',    [extra_files(['CommentsTest.hs']),
                     ignore_stderr], makefile_test, ['comments'])
test('InTreeAnnotations1',normal, compile, ['-dsuppress-uniques -ddump-parsed-ast'])
