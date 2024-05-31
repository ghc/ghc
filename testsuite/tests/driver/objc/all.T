test('objc-hi',
     [ unless(opsys('darwin'), skip),
       objc_src,
       expect_fail_for(['ghci']) ],
     compile_and_run, ['-framework Foundation'])

test('objcxx-hi',
     [ unless(opsys('darwin'), skip),
       objcxx_src,
       expect_fail_for(['ghci']) ],
     compile_and_run, ['-framework Foundation -lc++'])
