test('T10672_x64', [extra_clean(['cxxy.o',
                            'Main.exe', 'Main.hi', 'Main.o',
                            'Printf.o', 'Printf.hi']),
               [unless(opsys('mingw32'),skip) , unless(arch('x86_64'), skip)]],
               run_command, ['$MAKE -s --no-print-directory T10672_x64'])

test('T10672_x86', [extra_clean(['cxxy.o',
                            'Main.exe', 'Main.hi', 'Main.o',
                            'Printf.o', 'Printf.hi']),
               [unless(opsys('mingw32'),skip) , unless(arch('i386'), skip)]],
               run_command, ['$MAKE -s --no-print-directory T10672_x86'])
