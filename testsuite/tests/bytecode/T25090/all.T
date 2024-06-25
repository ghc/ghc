# This test compiles the boot file separately from its source file, which causes
# a debug assertion warning.
# Since this appears to be intentional according to the Note [Loading your own hi-boot file],
# the warning is added to the expected stderr for debugged builds.
def test_T25090(name):
    assert_warn_spec = {'stderr': 'T25090-debug.stderr'}
    extra_specs = assert_warn_spec if name == 'T25090a' and compiler_debugged() else {}
    return test(name,
     [extra_files(['A.hs', 'B.hs', 'C.hs-boot', 'C.hs', 'D.hs']),
      req_th,
      js_skip,
      use_specs(dict(stdout = 'T25090.stdout', **extra_specs)),
      ],
     makefile_test,
     [])

test_T25090('T25090a')

test_T25090('T25090b')
