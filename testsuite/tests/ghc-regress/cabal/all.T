setTestOpts(compose(alone,only_compiler_types(['ghc'])))

test('ghcpkg01',
     composes([extra_clean(['local01.package.conf',
                            'local01.package.conf.old']),
               skip_if_fast,
               if_compiler_lt('ghc', '6.9', skip)]),
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg01'])
clean(['local.package.conf', 'local.package.conf.old'])
test('ghcpkg02', skip_if_fast, run_command,
     ['$MAKE -s --no-print-directory ghcpkg02'])
clean(['package.conf.copy', 'package.conf.copy.old'])
test('ghcpkg03',
     extra_clean(['local03.package.conf',
                  'local03.package.conf.old']),
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg03'])
clean(['local.package.conf', 'local.package.conf.old'])
test('ghcpkg04',
     extra_clean(['local04.package.conf',
                  'local04.package.conf.old']),
     run_command,
     ['$MAKE -s --no-print-directory ghcpkg04'])
clean(['local.package.conf', 'local.package.conf.old'])

# Test that we *can* compile a module that also belongs to a package
# (this was disallowed in GHC 6.4 and earlier)
test('pkg01', normal, compile, [''])

test('1750',
     extra_clean(['1750.hs',
                  'local1750.package.conf',
                  'local1750.package.conf.old']),
     run_command, ['$MAKE -s --no-print-directory 1750'])

