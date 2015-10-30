test('T10890',
     extra_clean(['A.o', 'A.hi', 'B.o', 'B.hi']),
     multimod_compile, ['T10890', '-v0 -fwarn-unused-imports'])

test('T10890_1',
     extra_clean(['Base.o', 'Base.hi', 'Extends.o', 'Extends.hi']),
     multimod_compile, ['T10890_1', '-v0 -fwarn-unused-imports'])

test('T10890_2',
     extra_clean(['T10890_2A.o', 'T10890_2A.hi', 'T10890_2B.o', 'T10890_2B.hi']),
     multimod_compile, ['T10890_2', '-v0 -fwarn-unused-imports'])
