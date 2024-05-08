test('T24634',
     [extra_files(['hello.h', 'hello.c', 'Hello.hs', 'Main.hs']),
      req_c,
      req_th,
      ignore_stderr
      ],
     makefile_test,
     [])
