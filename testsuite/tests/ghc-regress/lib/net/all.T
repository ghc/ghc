
test('net001', normal, compile_and_run, ['-package network'])
test('net002', extra_run_opts('3'), compile_and_run, ['-package network'])

test('uri001', normal, compile_and_run, ['-package network -package HUnit'])
