test('T13002',
  [pre_cmd('chmod 444 .ghci'),
    only_ways(['ghci']),
    extra_files(['.ghci']),
    extra_run_opts('-fobject-code -ddump-rule-firings -no-ignore-dot-ghci')],
   ghci_script, ['T13002.script'])
