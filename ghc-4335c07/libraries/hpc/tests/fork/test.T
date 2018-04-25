setTestOpts([omit_ways(['ghci','threaded2']), when(fast(), skip)])

hpc_prefix = "perl hpcrun.pl --clear --exeext={exeext} --hpc={hpc}"

test('hpc_fork', [extra_files(['../hpcrun.pl']),
                   when(opsys('mingw32'), skip), # no forkProcess on Windows
                   cmd_prefix(hpc_prefix) ], compile_and_run, ['-fhpc'])

