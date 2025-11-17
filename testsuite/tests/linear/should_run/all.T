test('LinearTypeable', normal, compile_and_run, [''])
test('T26311', normal, compile_and_run, ['-O1'])
test('LinearGhci', normal, ghci_script, ['LinearGhci.script'])
