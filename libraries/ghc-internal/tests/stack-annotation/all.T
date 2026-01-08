# Javascript backend don't support annotation frames, yet
# and test with profiling way if available (#26507)
ann_frame_opts = [ when(js_arch(), skip)
                 , when(have_profiling(), extra_ways(['prof']))
		 , extra_files(['TestUtils.hs'])]

test('ann_frame001', ann_frame_opts, compile_and_run, [''])
test('ann_frame002', ann_frame_opts, compile_and_run, [''])
test('ann_frame003', ann_frame_opts, compile_and_run, [''])
test('ann_frame004', ann_frame_opts, compile_and_run, [''])
test('ann_frame005', ann_frame_opts, compile_and_run, [''])
