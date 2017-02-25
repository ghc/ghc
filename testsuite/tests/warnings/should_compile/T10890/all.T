test('T10890', [extra_files(['A.hs', 'B.hs'])], multimod_compile, ['T10890', '-v0 -fwarn-unused-imports'])

test('T10890_1', [extra_files(['Base.hs', 'Extends.hs'])], multimod_compile,
     ['T10890_1', '-v0 -fwarn-unused-imports'])

test('T10890_2', [], multimod_compile,
     ['T10890_2', '-v0 -fwarn-unused-imports'])
