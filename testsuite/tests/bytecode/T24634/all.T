def test_T24634(name):
    return test(name,
     [extra_files(['hello_c.h', 'hello_c.c', 'Hello.hs', 'Main.hs']),
      req_c,
      req_th,
      ],
     makefile_test,
     [])

test_T24634('T24634a')
test_T24634('T24634b')
