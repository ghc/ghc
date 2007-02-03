test('haddock.Test', compose(omit_ways(['opt', 'optasm', 'profasm']), normal), multimod_compile, ['Test Hidden Visible', '-fglasgow-exts -haddock -ddump-parsed'])
clean(['Test.hi', 'Test.o', 'Hidden.hi', 'Hidden.o', 'Visible.hi', 'Visible.o'])
