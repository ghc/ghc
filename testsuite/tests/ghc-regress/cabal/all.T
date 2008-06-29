setTestOpts(compose(alone,only_compiler_types(['ghc'])))

test('ghcpkg01',
     composes([extra_clean(['local01.package.conf',
                            'local01.package.conf.old']),
               skip_if_fast,
               if_compiler_lt('ghc', '6.9', skip)]),
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg01'])
test('ghcpkg02',
     [skip_if_fast,
      extra_clean(['package.conf.copy', 'package.conf.copy.old'])],
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

# Test that we *can* compile a module that also belongs to a package
# (this was disallowed in GHC 6.4 and earlier)
test('pkg01', normal, compile, [''])

test('1750',
     extra_clean(['1750.hs', '1750.out',
                  'local1750.package.conf',
                  'local1750.package.conf.old']),
     run_command, ['$MAKE -s --no-print-directory 1750'])

