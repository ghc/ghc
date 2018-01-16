# testcase for module import completions, e.g. `:complete repl "import Mod"`
test('prog015', extra_files(['Level1.hs', 'Level2/', 'TopLevel.hs']), ghci_script, ['prog015.script'])
