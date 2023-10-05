
setTestOpts(unless(config.stage == 1, skip))

# Test with stage1 compiler, see ticket:10382#comment:20.
test('T2632', normal, compile,
     ['-XTemplateHaskellQuotes -package template-haskell'])
test('T13609', normal, compile, [''])
