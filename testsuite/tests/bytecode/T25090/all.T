def test_T25090(name):
    return test(name,
     [extra_files(['A.hs', 'B.hs', 'C.hs-boot', 'C.hs', 'D.hs']),
      req_th,
      # TODO why does this not work?
      js_skip,
      use_specs({'stdout': 'T25090.stdout'}),
      ],
     makefile_test,
     [])

test_T25090('T25090a')

test_T25090('T25090b')
