test('T10138', [extra_files(['.keepme.hpc.T10138/']),
                ignore_stdout], run_command,
     # Using --hpcdir with an absolute path should work (exit code 0).
     ['{hpc} report T10138.keepme.tix --hpcdir="`pwd`/.keepme.hpc.T10138"'])

test('T11798', normal, makefile_test, [])

# Run tests below only for the hpc way.
#
# Do not explicitly specify '-fhpc' in extra_hc_opts, unless also setting
# '-hpcdir' to a different value for each test. Only the `hpc` way does this
# automatically. This way the tests in this directory can be run concurrently
# (Main.mix might overlap otherwise).
setTestOpts(only_ways(['hpc']))

def T2991(cmd):
  # The .mix file for the literate module should have non-zero entries.
  # The `grep` should exit with exit code 0.
  return(cmd + " && grep -q cover_me .hpc/T2991LiterateModule.mix")
test('T2991', [cmd_wrapper(T2991), extra_clean(['T2991LiterateModule.hi',
                                                'T2991LiterateModule.o'])],
              # Run with 'ghc --main'. Do not list other modules explicitly.
              multimod_compile_and_run, ['T2991', ''])

test('T17073', when(opsys('mingw32'), expect_broken(17607)),
     makefile_test, ['T17073 HPC={hpc}'])
