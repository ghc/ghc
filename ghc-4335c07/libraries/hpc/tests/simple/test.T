setTestOpts([omit_ways(['ghci']), when(fast(), skip)])

hpc_prefix = "perl hpcrun.pl --clear --exeext={exeext} --hpc={hpc}"

test('hpc001', [extra_files(['../hpcrun.pl']), cmd_prefix(hpc_prefix)],
     compile_and_run, ['-fhpc'])

