setTestOpts([
  # unless(doing_ghci, skip),
  unless(have_dynamic(),skip),
])

######################################
# https://gitlab.haskell.org/ghc/ghc/-/issues/23415

######################################
# When loading a Haskell symbol from a shared object (here `libHS_Reuse-1.0-ghcXXX.so` created from `Reuse.hs`),
# the native object loader in `loadNativeObj_ELF` stores the object code in a list.
# When the function is called again for the same object, the previously stored object code is returned.
test('T23415a',
     [extra_files(['Reuse.hs', 'Reuse.script', 'reuse.conf', 'ReusePlugin.hs']),
      grep_stderr('Found existing OC for.*libHSreuse-1.*'),
      req_rts_linker],
     makefile_test, ['T23415a'])

test('T23415b',
     [extra_files(['prepare-load.sh']),
      pre_cmd('$MAKE T23415b'),
      req_rts_linker,
      # extra_hc_opts('-package-db=db -fno-cache-loaded-library-units -v3')],
      extra_hc_opts('-package-db=db -fno-cache-loaded-library-units -v3')],
     ghci_script, ['T23415b.script'])

test('T23415c',
     [extra_files(['prepare-load.sh']),
      pre_cmd('$MAKE T23415b'),
      req_rts_linker],
     multimod_compile_and_run, ['Main', '-dynamic -package-db=db -O0'])

test('T23415d',
     [extra_files(['prepare-load.sh']),
      req_rts_linker],
     makefile_test, ['T23415d'])
