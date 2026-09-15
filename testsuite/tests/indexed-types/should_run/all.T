test('T2985', normal, compile_and_run, [''])
test('T4235', normal, compile_and_run, [''])

test('GMapAssoc', normal, compile_and_run, ['-package containers'])
test('GMapTop',   normal, compile_and_run, ['-package containers'])
test('T5719', normal, compile_and_run, [''])
test('T11465a', normal, compile_and_run, [''])

test('T27331', normal, compile_and_run, [''])
test('T27331b', [req_plugins, extra_files(['T27331b_Plugin.hs'])],
     multimod_compile_and_run, ['T27331b', '-package ghc ' + config.plugin_way_flags])
