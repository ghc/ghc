test('CallerCcParser', normal,
     compile_and_run,
     ['-package ghc'])

setTestOpts(req_profiling)
setTestOpts(extra_ways(['prof', 'ghci-ext-prof']))
setTestOpts(only_ways(prof_ways))
setTestOpts(extra_files(['Main.hs']))
setTestOpts(extra_run_opts('7'))
setTestOpts(grep_prof("Main.hs"))

# N.B. Main.hs is stolen from heapprof001.

test('CallerCc1', normal,
     multimod_compile_and_run,
     ['Main', '-fprof-callers=*.concat -O0'])

test('CallerCc2', normal,
     multimod_compile_and_run,
     ['Main', '-fprof-callers=Data.Foldable.concat -O0'])

test('CallerCc3', normal,
     multimod_compile_and_run,
     ['Main', '-fprof-callers=Data.Foldable.con*at -O0'])
