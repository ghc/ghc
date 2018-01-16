
test('annfail01', normal, compile_fail, [''])
test('annfail02', normal, compile_fail, [''])
test('annfail03', req_interp, compile_fail, [''])
test('annfail04', [extra_files(['Annfail04_Help.hs']),
                   req_interp], multimod_compile_fail, ['annfail04', '-v0'])
test('annfail05', [extra_files(['Annfail05_Help.hs']),
                   req_interp], multimod_compile_fail, ['annfail05', '-v0'])
test('annfail06', [extra_files(['Annfail06_Help.hs']),
                   req_interp], multimod_compile_fail, ['annfail06', '-v0'])
test('annfail07', req_interp, compile_fail, [''])
test('annfail08', req_interp, compile_fail, [''])
test('annfail09', req_interp, compile_fail, [''])
test('annfail10', req_interp, compile_fail, [''])
test('annfail11', normal, compile_fail, [''])
test('annfail12', req_interp, compile_fail, ['-v0'])
test('annfail13', normal, compile_fail, [''])
test('T10826', normal, compile_fail, [''])
""""
Helpful things to C+P:

test('', normal, compile_fail, [''])

test('', normal, compile, [''])

test('', [], multimod_compile_fail, ['', '-v0'])

test('', [], multimod_compile, ['', '-v0'])
"""
