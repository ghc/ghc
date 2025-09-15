def test_boot(name):
    return test(name,
     [extra_files(['A.hs', 'A.hs-boot', 'B.hs']),
      ],
     makefile_test,
     [])

test_boot('boot1')
test_boot('boot2')
test_boot('boot3')
