srcs = ['Mod.hs', 'Client.hs']
test('T19744', [run_timeout_multiplier(0.1),extra_files(srcs)], makefile_test, [])
