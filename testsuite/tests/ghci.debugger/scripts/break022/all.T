setTestOpts(extra_run_opts('-ignore-dot-ghci'))
setTestOpts(if_compiler_profiled(skip))

test('break022', extra_clean(['A.hs']), ghci_script, ['break022.script'])
