# Annotations were implemented in GHC 6.11 and not supported in other compilers
setTestOpts(if_compiler_lt('ghc', '6.11', skip));

test('ann01', normal, compile, ['-v0'])

""""
Helpful things to C+P:

test('', normal, compile_fail, [''])

test('', normal, compile, [''])

test('', extra_clean(['.hi', '.o']), 
     multimod_compile_fail, ['', '-v0'])

test('', extra_clean(['.hi', '.o']), 
     multimod_compile, ['', '-v0'])
"""