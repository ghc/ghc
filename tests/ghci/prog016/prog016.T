# testcase for regular identifier completions, e.g. `:complete repl "fun"`
test('prog016', extra_files(['Level1.hs', 'Level2/', 'TopLevel.hs']), ghci_script, ['prog016.script'])
