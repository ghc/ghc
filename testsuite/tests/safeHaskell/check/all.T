# check tests are about checking that the transitive safety
# check of safe haskell is working properly.

# Just do the normal way, SafeHaskell is all in the frontend
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

def normaliseBytestringPackage(str):
    return re.sub('bytestring-[0-9]+(\.[0-9]+)*', 'bytestring-<VERSION>', str)

test('CheckA', normal, compile, [''])
test('CheckB',
     extra_clean(['CheckB_Aux.hi', 'CheckB_Aux.o']),
     multimod_compile, ['CheckB', '-trust base'])

# fail as we don't trust base when compiling Check01
test('Check01', normal, multi_compile_fail, ['Check01 -distrust base', [
                                            ('Check01_A.hs', ''),
                                            ('Check01_B.hs', '-trust base')
                                            ], '-fpackage-trust'])

# suceed this time since we trust base when we compile AND use CheckB
test('Check02', normal, multi_compile, ['Check02', [
                                       ('Check02_A.hs', ''),
                                       ('Check02_B.hs', '')
                                       ], '-trust base -fpackage-trust'])

# suceed as while like Check01_fail we don't import CheckB as a safe
# import this time, so don't require base trusted when used.
test('Check03', normal, multi_compile, ['Check03', [
                                       ('Check03_A.hs', ''),
                                       ('Check03_B.hs', '-trust base')
                                       ], '-fpackage-trust'])

# Check a slightly larger transitive program. Check01 isn't imported
# safely by Check03 so we don't require base trused at end.
test('Check04', normal, multi_compile, ['Check04', [
                                       ('Check04_A.hs', ''),
                                       ('Check04_B.hs', '-trust base'),
                                       ('Check04_1.hs', '-trust base')
                                       ], '-fpackage-trust'])

# Check -fpackage-trust with no safe haskell flag is an error
test('Check05', normal, compile, [''])

# Check safe-infered modules have correct pkg trust requirements
test('Check06',
     extra_clean(['Check06_A.hi', 'Check06_A.o']),
     multimod_compile_fail, ['Check06', ''])

# Check selective safe imports bring in correct pkg trust requirements
test('Check07',
     extra_clean(['Check07_A.hi', 'Check07_A.o', 'Check07_B.hi', 'Check07_B.o']),
     multimod_compile, ['Check07', ''])

# Check selective safe imports bring in correct pkg trust requirements
test('Check08',
     extra_clean(['Check08_A.hi', 'Check08_A.o', 'Check08_B.hi', 'Check08_B.o']),
     multimod_compile_fail, ['Check08', ''])

# check -distrust-all-packages flag works
test('Check09',
     normalise_errmsg_fun(normaliseBytestringPackage),
     compile_fail,
     ['-fpackage-trust -distrust-all-packages'])

# as above but trust this time
test('Check10', normal, compile,
     ['-fpackage-trust -distrust-all-packages -trust bytestring -trust base'])

