# trac #1545: GHCi debugger doesn't work on an unregisterised compiler
def f(opts):
  if config.unregisterised:
    opts.skip = 1
setTestOpts(f)

setTestOpts(extra_run_opts('-ignore-dot-ghci'))


test('break022', normal, ghci_script, ['break022.script'])
