# Historically this scenario resulted in a second error:
# 
#    Identifier ‘f’ has conflicting definitions in the module
#    and its hs-boot file
#    Main module: f :: T -> T
#    Boot file:   f :: T -> T
#    The two types are different
test('T13180', normal, multimod_compile_fail, ['T13180', '-fforce-recomp'])
