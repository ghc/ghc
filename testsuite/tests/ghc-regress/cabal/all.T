setTestOpts(compose(alone,only_compiler_types(['ghc'])))

test('ghcpkg01', skip_if_fast, run_command_ignore_output, ['$MAKE ghcpkg01'])
clean(['local.package.conf', 'local.package.conf.old'])
test('ghcpkg02', skip_if_fast, run_command_ignore_output, ['$MAKE ghcpkg02'])
clean(['package.conf.copy', 'package.conf.copy.old'])
test('ghcpkg03', normal, run_command_ignore_output, ['$MAKE ghcpkg03'])
clean(['local.package.conf', 'local.package.conf.old'])
test('ghcpkg04', normal, run_command, ['$MAKE --no-print-directory ghcpkg04'])
clean(['local.package.conf', 'local.package.conf.old'])

# Test that we *can* compile a module that also belongs to a package
# (this was disallowed in GHC 6.4 and earlier)
test('pkg01', normal, compile, [''])
