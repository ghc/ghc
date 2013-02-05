# Just do the normal way, SafeHaskell is all in the frontend
def f( opts ):
  opts.only_ways = ['normal']

def normaliseArrayPackage(str):
    return re.sub('array-[0-9]+(\.[0-9]+)*', 'array-<VERSION>', str)

def normaliseBytestringPackage(str):
    return re.sub('bytestring-[0-9]+(\.[0-9]+)*', 'bytestring-<VERSION>', str)

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

if config.have_shared_libs:
    dyn = '--enable-shared'
else:
    dyn = '--disable-shared'

make_args = 'VANILLA=' + vanilla + ' PROF=' + prof + ' DYN=' + dyn

# Test building a package, that trust values are set correctly
# and can be changed correctly
test('safePkg01',
     [clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.safePkg01'),
      normalise_errmsg_fun(ignoreLdOutput),
      normalise_fun(two_normalisers(normaliseArrayPackage,
                                    normaliseBytestringPackage))],
     run_command, 
     ['$MAKE -s --no-print-directory safePkg01 ' + make_args])

# Fail since we enable package trust
test('ImpSafe01', normal, compile_fail, ['-fpackage-trust -distrust base'])

# Succeed since we don't enable package trust
test('ImpSafe02', normal, compile, ['-distrust base'])

test('ImpSafeOnly01',
     [pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly01 ' + make_args),
      clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.ImpSafeOnly01')],
     compile,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly01/local.db -trust base'])

test('ImpSafeOnly02',
     [pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly02 ' + make_args),
      clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.ImpSafeOnly02')],
     compile,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly02/local.db -trust base -trust safePkg01'])

# Fail since we enable package trust (and still need safePkg01 trusted)
test('ImpSafeOnly03',
     [pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly03 ' + make_args),
      clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.ImpSafeOnly03')],
     compile_fail,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly03/local.db -trust base'])

# Succeed since we don't enable package trust
test('ImpSafeOnly04',
     [pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly04 ' + make_args),
      clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.ImpSafeOnly04')],
     compile,
     ['-package-db pdb.ImpSafeOnly04/local.db -trust base'])

# fail due to missing trust of safePkg01, next test succeeds.
test('ImpSafeOnly05',
     [pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly05 ' + make_args),
      clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.ImpSafeOnly05')],
     compile_fail,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly05/local.db -trust base'])
test('ImpSafeOnly06',
     [pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly06 ' + make_args),
      clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.ImpSafeOnly06')],
     compile,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly06/local.db -trust base -trust safePkg01'])

# fail due to missing trust
test('ImpSafeOnly07',
     [pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly07 ' + make_args),
      clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.ImpSafeOnly07'),
      normalise_errmsg_fun(normaliseBytestringPackage)],
     compile_fail,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly07/local.db -trust safePkg01'])
test('ImpSafeOnly08',
     [pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly08 ' + make_args),
      clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.ImpSafeOnly08'),
      normalise_errmsg_fun(normaliseBytestringPackage)],
     compile_fail,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly08/local.db -trust safePkg01'])
test('ImpSafeOnly09',
     [pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly09 ' + make_args),
      clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.ImpSafeOnly09'),
      normalise_errmsg_fun(normaliseBytestringPackage)],
     compile_fail,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly09/local.db -trust safePkg01'])

# finally succeed
test('ImpSafeOnly10',
     [pre_cmd('$MAKE -s --no-print-directory mkPackageDatabase.ImpSafeOnly10 ' + make_args),
      clean_cmd('$MAKE -s --no-print-directory cleanPackageDatabase.ImpSafeOnly10')],
     compile,
     ['-fpackage-trust -package-db pdb.ImpSafeOnly10/local.db -trust safePkg01 -trust base -trust bytestring'])

