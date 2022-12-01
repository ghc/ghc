test('Chan002', [extra_run_opts('100'), fragile(22836)], compile_and_run, [''])
test('Chan003',      extra_run_opts('200'), compile_and_run, [''])
test('ThreadDelay001', js_broken(22374), compile_and_run, [''])
