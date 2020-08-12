setTestOpts(req_profiling)
setTestOpts(extra_ways(['prof', 'ghci-ext-prof']))
setTestOpts(only_ways(prof_ways))

test('CallerCc1', extra_files(['Main.hs']), compile_and_run, ['-fprof-callers=Data.List.map'])
test('CallerCc2', extra_files(['Main.hs']), compile_and_run, ['-fprof-callers=*.map'])
test('CallerCc3', extra_files(['Main.hs']), compile_and_run, ['-fprof-callers=Data.List.con*t'])
