accept = 'ACCEPT=--accept' if config.accept else 'ACCEPT=""'

test('haddockHtmlTest',
     [ignore_stdout, ignore_stderr, unless(in_tree_compiler(), skip), req_haddock],
     makefile_test,
     ['htmlTest ' + accept])

test('haddockLatexTest',
     [ignore_stdout, ignore_stderr, unless(in_tree_compiler(), skip), req_haddock],
     makefile_test,
     ['latexTest ' + accept])

test('haddockHoogleTest',
     [ignore_stdout, ignore_stderr, unless(in_tree_compiler(), skip), req_haddock],
     makefile_test,
     ['hoogleTest ' + accept])

test('haddockHypsrcTest',
     [ignore_stdout, ignore_stderr, unless(in_tree_compiler(), skip), req_haddock],
     makefile_test,
     ['hypsrcTest ' + accept])
