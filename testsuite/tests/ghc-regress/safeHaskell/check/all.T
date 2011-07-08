# Just do the normal way, SafeHaskell is all in the frontend
def f( opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('CheckA', normal, compile, [''])
test('CheckB', normal, multimod_compile, ['CheckB', '-trust base'])

# fail as we don't trust base when compiling Check01
test('Check01', normal, multi_compile_fail, ['Check01', [
                                            ('Check01_A.hs', ''),
                                            ('Check01_B.hs', '-trust base')
                                            ], ''])

# suceed this time since we trust base when we compile AND use CheckB
test('Check02', normal, multi_compile, ['Check02', [
                                       ('Check02_A.hs', ''),
                                       ('Check02_B.hs', '')
                                       ], '-trust base'])

# suceed as while like Check01_fail we don't import CheckB as a safe
# import this time, so don't require base trusted when used.
test('Check03', normal, multi_compile, ['Check03', [
                                       ('Check03_A.hs', ''),
                                       ('Check03_B.hs', '-trust base')
                                       ], ''])

# Check a slightly larger transitive program. Check01 isn't imported
# safely by Check03 so we don't require base trused at end.
test('Check04', normal, multi_compile, ['Check04', [
                                       ('Check04_A.hs', ''),
                                       ('Check04_B.hs', '-trust base'),
                                       ('Check04_1.hs', '-trust base')
                                       ], ''])

