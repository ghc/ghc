# DEMO for #15963: every test here times out on purpose, at a different step,
# to show how the driver reports timeouts. Not meant to be merged.

# ceil(config.timeout * 0.01) = 3s with the default 300s timeout.
setTestOpts([compile_timeout_multiplier(0.01),
             run_timeout_multiplier(0.01),
             pre_cmd_timeout_multiplier(0.01)])

test('Slow', req_th, compile, [''])
test('SlowFail', req_th, compile_fail, [''])
test('SlowRun', normal, compile_and_run, [''])
test('PreCmd', pre_cmd('sleep 60'), compile, [''])
