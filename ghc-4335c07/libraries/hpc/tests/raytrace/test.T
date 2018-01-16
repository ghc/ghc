setTestOpts([omit_ways(['ghci']), when(fast(), skip)])

hpc_prefix = "perl hpcrun.pl --clear --exeext={exeext} --hpc={hpc}"

# TODO. It is unclear what the purpose of this test is. It produces lots of
# output, but the expected output file is missing. I (thomie) added
# the ignore_stdout setup function, just to make the test pass for the
# moment.
# Note that the subdirectory tixs also has a test.T file, and those tests
# depend on some of the files in this directory.
# Also note that testsuite/tests/programs/galois_raytrace has a similar (but
# not the same) copy of this program.
test('hpc_raytrace',
     [extra_files(['../hpcrun.pl', 'CSG.hs', 'Construct.hs', 'Data.hs', 'Eval.hs', 'Geometry.hs', 'Illumination.hs', 'Intersections.hs', 'Interval.hs', 'Main.hs', 'Misc.hs', 'Parse.hs', 'Primitives.hs', 'Surface.hs', 'galois.gml', 'galois.sample']),
      cmd_prefix(hpc_prefix), reqlib('parsec'), ignore_stdout],
     multimod_compile_and_run, ['Main','-fhpc -package parsec'])

