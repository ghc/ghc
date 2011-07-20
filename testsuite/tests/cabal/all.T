setTestOpts(only_compiler_types(['ghc']))

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
     extra_clean(['local03.package.conf',
                  'local03.package.conf.old']),
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
       normalise_fun(normalise_haddock_junk)
       ],
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg05'])
test('ghcpkg06',
     [extra_clean(['local06.package.conf',
                   'local06.package.conf.old'])],
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg06'])

# Test that we *can* compile a module that also belongs to a package
# (this was disallowed in GHC 6.4 and earlier)
test('pkg01', normal, compile, [''])

test('1750',
     extra_clean(['1750.hs', '1750.out',
                  'local1750.package.conf',
                  'local1750.package.conf.old']),
     run_command, ['$MAKE -s --no-print-directory 1750'])

test('shadow',
     extra_clean(['shadow.out', 'shadow.hs', 'shadow.hi',
                  'local1shadow1.package.conf',
                  'local1shadow1.package.conf.old',
                  'local1shadow2.package.conf',
                  'local1shadow2.package.conf.old']),
     run_command, ['$MAKE -s --no-print-directory shadow'])
