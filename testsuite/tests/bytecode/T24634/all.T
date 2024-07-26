test('T24634',
     [extra_files(['hello_c.h', 'hello_c.c', 'Hello.hs', 'Main.hs']),
      req_c,
      req_th,
      ignore_stderr
      ],
     makefile_test,
     [])
