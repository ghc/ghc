def normaliseDynlibNames(str):
    return re.sub('-ghc[0-9.]+\.', '-ghc<VERSION>.', str)

def ignore_warnings(str):
    return re.sub(r'Warning:.*\n', '', str)

test('ghcpkg01', [extra_files(['test.pkg', 'test2.pkg', 'test3.pkg'])], run_command, ['$MAKE -s --no-print-directory ghcpkg01'])

# Use ignore_stderr to prevent (when HADDOCK_DOCS=NO):
#  warning: haddock-interfaces .. doesn't exist or isn't a file
test('ghcpkg02', [ignore_stderr], run_command,
     ['$MAKE -s --no-print-directory ghcpkg02'])

test('ghcpkg03', [extra_files(['test.pkg', 'test2.pkg', 'test4.pkg']),
                  normalise_errmsg_fun(normaliseDynlibNames)], run_command,
     ['$MAKE -s --no-print-directory ghcpkg03'])

def normalise_package_order(s):
    # Package order is not deterministic?
    return re.sub('testpkg-1.2.3.4 newtestpkg-2.0',
                  'newtestpkg-2.0 testpkg-1.2.3.4',
                  s)

test('ghcpkg04', [extra_files(['test.pkg', 'test5.pkg']),
                  normalise_errmsg_fun(normalise_package_order)], run_command,
     ['$MAKE -s --no-print-directory ghcpkg04'])

# Sometimes we get spurious warnings from ghc-pkg about missing
# haddock-interfaces; this filters them out.
def normalise_haddock_junk( str ):
    return re.sub(r'Warning: haddock.*\n', '', str)

test('ghcpkg05',
     [extra_files(['test2.pkg', 'test3.pkg']),
      normalise_errmsg_fun(normalise_haddock_junk, normaliseDynlibNames, ignore_warnings)],
     run_command, ['$MAKE -s --no-print-directory ghcpkg05'])
test('ghcpkg06', [extra_files(['test.pkg', 'testdup.pkg'])], run_command, ['$MAKE -s --no-print-directory ghcpkg06'])

test('ghcpkg07', [extra_files(['test.pkg', 'test7a.pkg', 'test7b.pkg'])], run_command, ['$MAKE -s --no-print-directory ghcpkg07'])

# Test that we *can* compile a module that also belongs to a package
# (this was disallowed in GHC 6.4 and earlier)
test('pkg01', normal, compile, [''])

test('T1750', [], run_command, ['$MAKE -s --no-print-directory T1750'])

test('T5442a', [extra_files(['test.pkg'])], run_command, ['$MAKE -s --no-print-directory T5442a'])

test('T5442b', [extra_files(['test.pkg'])], run_command, ['$MAKE -s --no-print-directory T5442b'])

test('T5442c', [extra_files(['test.pkg'])], run_command, ['$MAKE -s --no-print-directory T5442c'])

test('T5442d', [extra_files(['shadow1.pkg', 'shadow2.pkg', 'shadow4.pkg'])], run_command, ['$MAKE -s --no-print-directory T5442d'])

test('shadow', [], run_command, ['$MAKE -s --no-print-directory shadow'])

test('T12485a', [extra_files(['shadow1.pkg', 'shadow2.pkg', 'shadow3.pkg'])], run_command, ['$MAKE -s --no-print-directory T12485a'])

test('T13703', [extra_files(['test13703a.pkg', 'test13703b.pkg'])], run_command, ['$MAKE -s --no-print-directory T13703'])
