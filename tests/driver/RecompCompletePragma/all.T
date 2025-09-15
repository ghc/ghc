test('RecompCompletePragma', [copy_files, extra_files(["A1.hs", "A2.hs", "A3.hs", "A4.hs"])],
     makefile_test,
     ['RecompCompletePragma'])

test('RecompCompleteFixity', [copy_files, extra_files(["B1.hs"])],
     makefile_test,
     ['RecompCompleteFixity'])

test('RecompCompleteIndependence', [copy_files, extra_files(["C1.hs", "C2.hs", "C3.hs"])],
     makefile_test,
     ['RecompCompleteIndependence'])
