# Just do the normal way, SafeHaskell is all in the frontend
def f( opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('CheckA', normal, compile, [''])
test('CheckB', normal, multimod_compile, ['CheckB', '-trust base'])

# fail as we don't trust base when compiling Check01
test('Check01_fail', normal, multi_compile_fail, ['Check01', [
                                                 ('CheckA.hs', ''),
                                                 ('CheckB.hs', '-trust base')
                                                 ], ''])

# suceed this time since we trust base when we compile AND use CheckB
test('Check01_succeed', normal, multi_compile, ['Check01', [
                                               ('CheckA.hs', ''),
                                               ('CheckB.hs', '')
                                               ], '-trust base'])

# suceed as while like Check01_fail we don't import CheckB as a safe
# import this time, so don't require base trusted when used.
test('Check02', normal, multi_compile, ['Check02', [
                                       ('CheckA.hs', ''),
                                       ('CheckB.hs', '-trust base')
                                       ], ''])

# Check a slightly larger transitive program. Check01 isn't imported
# safely by Check03 so we don't require base trused at end.
test('Check03', normal, multi_compile, ['Check03', [
                                       ('CheckA.hs', ''),
                                       ('CheckB.hs', '-trust base'),
                                       ('Check01.hs', '-trust base')
                                       ], ''])

