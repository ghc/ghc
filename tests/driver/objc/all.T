test('objc-hi',
     [ unless(opsys('darwin'), skip),
       when(config.target_wrapper is not None, skip),
       objc_src,
       expect_fail_for(['ghci']) ],
     compile_and_run, ['-framework Foundation'])

test('objcxx-hi',
     [ unless(opsys('darwin'), skip),
       when(config.target_wrapper is not None, skip),
       objcxx_src,
       expect_fail_for(['ghci']) ],
     compile_and_run, ['-framework Foundation -lc++'])
