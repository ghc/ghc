setTestOpts(only_compiler_types(['ghc']))

def normaliseDynlibNames(str):
    return re.sub('-ghc[0-9.]+\.', '-ghc<VERSION>.', str)

test('ghcpkg01',
     extra_clean(['local01.package.conf',
                  'local01.package.conf.old']),
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg01'])
test('ghcpkg02',
     [ignore_output,
      extra_clean(['package.conf.ghcpkg02', 'package.conf.ghcpkg02.old'])],
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg02'])
test('ghcpkg03',
     [extra_clean(['local03.package.conf',
                   'local03.package.conf.old']),
      normalise_errmsg_fun(normaliseDynlibNames)],
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg03'])
test('ghcpkg04',
     [ignore_output,
      extra_clean(['local04.package.conf',
                   'local04.package.conf.old'])],
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg04'])

# Sometimes we get spurious warnings from ghc-pkg about missing
# haddock-interfaces; this filters them out.
def normalise_haddock_junk( str ):
    return re.sub(r'Warning: haddock.*\n', '', str)

test('ghcpkg05',
     [ extra_clean(['local05a.package.conf',
                    'local05a.package.conf.old',
                    'local05b.package.conf',
                    'local05b.package.conf.old']),
       normalise_errmsg_fun(two_normalisers(normalise_haddock_junk,
                                            normaliseDynlibNames))
       ],
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg05'])
test('ghcpkg06',
     [extra_clean(['local06.package.conf',
                   'local06.package.conf.old'])],
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg06'])

test('ghcpkg07',
     extra_clean(['local07.package.conf',
                  'local07.package.conf.old']),
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg07'])

# Test that we *can* compile a module that also belongs to a package
# (this was disallowed in GHC 6.4 and earlier)
test('pkg01', normal, compile, [''])

test('T1750',
     extra_clean(['T1750.hs', 'T1750.out',
                  'localT1750.package.conf',
                  'localT1750.package.conf.old']),
     run_command, ['$MAKE -s --no-print-directory T1750'])

test('T5442a',
     [extra_clean(['package.conf.T5442a.global', 'package.conf.T5442a.user'])],
     run_command,
     ['$MAKE -s --no-print-directory T5442a'])

test('T5442b',
     [extra_clean(['package.conf.T5442b.global', 'package.conf.T5442b.user'])],
     run_command,
     ['$MAKE -s --no-print-directory T5442b'])

test('T5442c',
     [extra_clean(['package.conf.T5442c.global', 'package.conf.T5442c.user',
                   'package.conf.T5442c.extra'])],
     run_command,
     ['$MAKE -s --no-print-directory T5442c'])

test('T5442d',
     [extra_clean(['package.conf.T5442d.global', 'package.conf.T5442d.user',
                   'package.conf.T5442d.extra'])],
     run_command,
     ['$MAKE -s --no-print-directory T5442d'])

test('shadow',
     extra_clean(['shadow.out', 'shadow.hs', 'shadow.hi',
                  'local1shadow1.package.conf',
                  'local1shadow1.package.conf.old',
                  'local1shadow2.package.conf',
                  'local1shadow2.package.conf.old']),
     run_command, ['$MAKE -s --no-print-directory shadow'])
