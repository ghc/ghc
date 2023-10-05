test('THPutDocExternal',
     req_th,
     multimod_compile_fail,
     ['THPutDocExternal', '-no-hs-main -haddock -c -v0'])

test('THPutDocNonExistent',
     req_th,
     multimod_compile_fail,
     ['THPutDocNonExistent', '-no-hs-main -haddock -c -v0'])
