# testcase for bug #936
test('ghci.prog007', extra_files(['A.hs', 'B.hs', 'C.hs', 'C.hs-boot']), ghci_script, ['ghci.prog007.script'])
