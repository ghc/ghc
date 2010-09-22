
test('annfail01', normal, compile_fail, [''])
test('annfail02', normal, compile_fail, [''])
test('annfail03', normal, compile_fail, [''])
test('annfail04', extra_clean(['Annfail04_Help.hi', 'Annfail04_Help.o']), 
     multimod_compile_fail, ['annfail04', '-v0'])
test('annfail05', extra_clean(['Annfail05_Help.hi', 'Annfail05_Help.o']), 
     multimod_compile_fail, ['annfail05', '-v0'])
test('annfail06', extra_clean(['Annfail06_Help.hi', 'Annfail06_Help.o']), 
     multimod_compile_fail, ['annfail06', '-v0'])
test('annfail07', normal, compile_fail, [''])
test('annfail08', normal, compile_fail, [''])
test('annfail09', normal, compile_fail, [''])
test('annfail10', normal, compile_fail, [''])
test('annfail11', normal, compile_fail, [''])
test('annfail12', normal, compile_fail, ['-v0'])
test('annfail13', normal, compile_fail, [''])

""""
Helpful things to C+P:

test('', normal, compile_fail, [''])

test('', normal, compile, [''])

test('', extra_clean(['.hi', '.o']), 
     multimod_compile_fail, ['', '-v0'])

test('', extra_clean(['.hi', '.o']), 
     multimod_compile, ['', '-v0'])
"""
