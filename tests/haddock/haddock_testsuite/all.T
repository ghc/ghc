accept = 'ACCEPT=--accept' if config.accept else 'ACCEPT=""'

test('haddockHtmlTest',
     [ignore_stdout, ignore_stderr, req_haddock],
     makefile_test,
     ['htmlTest ' + accept])

test('haddockLatexTest',
     [ignore_stdout, ignore_stderr, req_haddock],
     makefile_test,
     ['latexTest ' + accept])

test('haddockHoogleTest',
     [ignore_stdout, ignore_stderr, req_haddock],
     makefile_test,
     ['hoogleTest ' + accept])

test('haddockHypsrcTest',
     [ignore_stdout, ignore_stderr, req_haddock],
     makefile_test,
     ['hypsrcTest ' + accept])

test('haddockForeignTest',
     [ignore_stdout, ignore_stderr, req_haddock, extra_files(['./haddock-th-foreign-repro/A.hs', './haddock-th-foreign-repro/B.hs', './haddock-th-foreign-repro/F.hs', './haddock-th-foreign-repro/arith.c'])],
     makefile_test,
     ['haddockForeignTest'])

test('T26114',
   [ignore_stderr, req_haddock, extra_files(['T26114.hs'])],
   makefile_test,
   ['T26114'])
