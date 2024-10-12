# Just do the normal way, SafeHaskell is all in the frontend
def f( name, opts ):
  opts.only_ways = ['normal']

def ignoreLdOutput(str):
    return re.sub('Creating library file: pdb.safePkg01/dist.build.libHSsafePkg01-1.0-ghc[0-9.]*.dll.a\n', '', str)

setTestOpts(f)

if config.have_vanilla:
    vanilla = '--enable-library-vanilla'
else:
    vanilla = '--disable-library-vanilla'

if config.have_profiling:
    prof = '--enable-library-profiling'
else:
    prof = '--disable-library-profiling'

if config.have_dynamic:
    dyn = '--enable-shared'
else:
    dyn = '--disable-shared'

make_args = 'VANILLA=' + vanilla + ' PROF=' + prof + ' DYN=' + dyn

# Test building a package, that trust values are set correctly
# and can be changed correctly
test('safePkg01',
     [extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      normalise_errmsg_fun(ignoreLdOutput, normalise_errmsg),
      normalise_version("array", "ghc-bignum", "bytestring",
                        "base", "deepseq", "ghc-prim"),
      normalise_fun(normalise_errmsg),
      js_broken(22349)],
     run_command, ['$MAKE -s --no-print-directory safePkg01 ' + make_args])

# Fail since we enable package trust
test('ImpSafe01', normalise_version('base'), compile_fail, ['-fpackage-trust -distrust base'])

# Succeed since we don't enable package trust
test('ImpSafe02', normal, compile, ['-distrust base'])

# Fail since we don't trust base of bytestring
test('ImpSafe03',
  [normalise_version("bytestring")],
  multi_compile_fail,
  ['ImpSafe03 -trust base -distrust bytestring', [
   ('ImpSafe03_A.hs', ' -trust base -trust bytestring')
  ], '-fpackage-trust' ])

# Fail same as ImpSafe01 but testing with -XTrustworthy now
test('ImpSafe04', normalise_version('base'), compile_fail, ['-fpackage-trust -distrust base'])

test('ImpSafeOnly01',
     [req_host_target_ghc,
      extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly01 ' + make_args)],
     compile,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly01/local.db -trust base'])

test('ImpSafeOnly02',
     [req_host_target_ghc,
      extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly02 ' + make_args)],
     compile,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly02/local.db -trust base -trust safePkg01'])

# Fail since we enable package trust (and still need safePkg01 trusted)
test('ImpSafeOnly03',
     [req_host_target_ghc,
      extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly03 ' + make_args)],
     compile_fail,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly03/local.db -trust base'])

# Succeed since we don't enable package trust
test('ImpSafeOnly04',
     [req_host_target_ghc,
      extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly04 ' + make_args)],
     compile, ['-package-db pdb.ImpSafeOnly04/local.db -trust base'])

# fail due to missing trust of safePkg01, next test succeeds.
test('ImpSafeOnly05',
     [req_host_target_ghc,
      extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly05 ' + make_args)],
     compile_fail,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly05/local.db -trust base'])
test('ImpSafeOnly06',
     [req_host_target_ghc,
      extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly06 ' + make_args)],
     compile,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly06/local.db -trust base -trust safePkg01'])

# fail due to missing trust
test('ImpSafeOnly07',
     [req_host_target_ghc,
      extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly07 ' + make_args),
      normalise_version("bytestring", "base")],
     compile_fail,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly07/local.db -trust safePkg01 -distrust bytestring'])
test('ImpSafeOnly08',
     [req_host_target_ghc,
      extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly08 ' + make_args),
      normalise_version("bytestring", "base")],
     compile_fail,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly08/local.db -trust safePkg01'])
test('ImpSafeOnly09',
     [req_host_target_ghc,
      extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly09 ' + make_args),
      normalise_version("bytestring")],
     compile_fail,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly09/local.db -trust safePkg01'])

# finally succeed
test('ImpSafeOnly10',
     [req_host_target_ghc,
      extra_files(['M_SafePkg.hs', 'M_SafePkg2.hs', 'M_SafePkg3.hs', 'M_SafePkg4.hs', 'M_SafePkg5.hs', 'M_SafePkg6.hs', 'M_SafePkg7.hs', 'M_SafePkg8.hs', 'Setup.hs', 'p.cabal']),
      pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly10 ' + make_args)],
     compile,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly10/local.db -trust safePkg01 -trust base -trust bytestring'])
