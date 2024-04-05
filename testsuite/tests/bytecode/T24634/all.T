test('T24634',
     [extra_files(['hello.h', 'hello.c', 'Hello.hs', 'Main.hs']),
      req_interp,
      expect_broken(24634),
      ],
     makefile_test,
     [''])
