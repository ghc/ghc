if config.have_vanilla:
    vanilla = '--enable-library-vanilla'
else:
    vanilla = '--disable-library-vanilla'

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = '--disable-library-profiling'

if config.have_shared_libs:
    dyn = '--enable-shared'
else:
    dyn = '--disable-shared'

if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

def ignoreLdOutput(str):
    return re.sub('Creating library file: dist.build.libHStest-1.0-ghc[0-9.]*.dll.a\n', '', str)

test('cabal01',
     [extra_files(['A.hs', 'B/', 'MainA.hs', 'Setup.lhs', 'c_src/', 'hello.c', 'test.cabal']),
      normalise_errmsg_fun(ignoreLdOutput)],
     run_command,
     ['$MAKE -s --no-print-directory cabal01 VANILLA=' + vanilla + ' PROF=' + prof + ' DYN=' + dyn + ' ' + cleanup])
