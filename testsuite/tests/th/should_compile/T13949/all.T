test('T13949', extra_files(['ASCII.hs', 'PatternGenerator.hs', 'These.hs', 'Tree.hs']),
     multimod_compile, ['ASCII PatternGenerator These Tree', '-fno-code -v0'])