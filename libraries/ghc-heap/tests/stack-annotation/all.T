# Javascript backend don't support annotation frames, yet
setTestOpts(when(js_arch(), skip))

test('ann_frame001', [extra_files(['TestUtils.hs'])], compile_and_run, [''])
test('ann_frame002', [extra_files(['TestUtils.hs'])], compile_and_run, [''])
test('ann_frame003', [extra_files(['TestUtils.hs'])], compile_and_run, [''])
test('ann_frame004', [extra_files(['TestUtils.hs'])], compile_and_run, [''])
