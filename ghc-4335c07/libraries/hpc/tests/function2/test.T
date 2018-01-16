setTestOpts([omit_ways(['ghci']), when(fast(), skip)])

hpc_prefix = "perl hpcrun.pl --clear --exeext={exeext} --hpc={hpc}"

# Test that -fhpc still works when (a) the source file is in a
# subdirectory and (b) it is a literate file.  We had a bug in this
# area caused by the handling of line pragmas in the preprocessed
# source file.
test('tough2',
     [extra_files(['../hpcrun.pl', 'subdir/']),
       literate,
       cmd_prefix(hpc_prefix),
       omit_ways(['ghci'] + prof_ways) ], # profile goes in the wrong place
     multimod_compile_and_run, ['subdir/tough2.lhs', '-fhpc'])
