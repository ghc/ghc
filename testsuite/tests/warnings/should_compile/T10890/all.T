test('T10890',
     extra_clean(['A.o', 'A.hi', 'B.o', 'B.hi']),
     multimod_compile, ['T10890', '-v0 -fwarn-unused-imports'])

test('T10890_1',
     extra_clean(['Base.o', 'Base.hi', 'Extends.o', 'Extends.hi']),
     multimod_compile, ['T10890_1', '-v0 -fwarn-unused-imports'])
