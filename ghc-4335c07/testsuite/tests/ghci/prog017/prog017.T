# testcase for operator completions, e.g. `:complete repl ">>"`
test('prog017', extra_files(['Level1.hs', 'Level2/', 'TopLevel.hs']), ghci_script, ['prog017.script'])
