test('RecompCompletePragma', [copy_files, extra_files(["A1.hs", "A2.hs", "A3.hs", "A4.hs"])],
     makefile_test,
     ['RecompCompletePragma'])

test('RecompCompleteFixity', [copy_files, extra_files(["B1.hs"])],
     makefile_test,
     ['RecompCompleteFixity'])
