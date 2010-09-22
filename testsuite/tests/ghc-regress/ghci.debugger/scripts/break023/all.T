setTestOpts(extra_run_opts('-ignore-dot-ghci'))
setTestOpts(if_compiler_profiled(skip))

test('break023', normal, ghci_script, ['break023.script'])
