test('T24275', extra_files(['T24275A.hs','T24275A.hs-boot','T24275B.hs-boot','T24275B.hs']), multimod_compile_fail, ['T24275B',''])
