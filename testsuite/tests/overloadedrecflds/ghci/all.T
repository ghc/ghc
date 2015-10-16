setTestOpts(when(compiler_profiled(), skip))

test('overloadedrecfldsghci01', combined_output, ghci_script, ['overloadedrecfldsghci01.script'])
