# trac #1545: GHCi debugger doesn't work on an unregisterised compiler
def f(opts):
  if config.unregisterised:
    opts.skip = 1
setTestOpts(f)

setTestOpts(extra_run_opts('-ignore-dot-ghci'))


test('break023', if_compiler_ge('ghc', '6.9', expect_broken(2034)),
                 ghci_script, ['break023.script'])
