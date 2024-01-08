def if_not_platform(platforms, f):
    if not (config.platform in platforms):
       return f
    else:
       return normal

skip_if_not_osx = if_not_platform(['i386-apple-darwin','x86_64-apple-darwin'], skip)

test('objc-hi',
     [ skip_if_not_osx,
       objc_src,
       expect_fail_for(['ghci']) ],
     compile_and_run, ['-framework Foundation'])

test('objcxx-hi',
     [ skip_if_not_osx,
       objcxx_src,
       expect_fail_for(['ghci']) ],
     compile_and_run, ['-framework Foundation -lc++'])
