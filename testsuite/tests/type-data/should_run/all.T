test('T22332a', exit_code(1), compile_and_run, [''])
test('T22315b', extra_files(['T22315b.hs']), ghci_script, ['T22315b.script'])
