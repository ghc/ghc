test('haddockHtmlTest'  , [ignore_stdout], makefile_test, ['htmlTest'  ])
test('haddockLatexTest' , [ignore_stdout], makefile_test, ['latexTest' ])
test('haddockHoogleTest', [ignore_stdout], makefile_test, ['hoogleTest'])
test('haddockHypsrcTest', [ignore_stdout], makefile_test, ['hypsrcTest'])
