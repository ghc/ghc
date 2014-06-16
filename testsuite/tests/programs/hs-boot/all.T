# This test is for bug #906, a problem with the ordering of
# compilations involving .hs-boot files, fixed in 6.6.

test('hs-boot',
     extra_clean(['A.hi', 'A.hi-boot', 'A.o', 'A.o-boot',
                  'B.hi', 'B.o', 'C.hi', 'C.o',
                  'Main', 'Main.exe', 'Main.hi', 'Main.o']),
     multimod_compile,
     ['Main -v0', ''])
