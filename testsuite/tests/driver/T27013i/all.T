test('T27013i', normalise_version('containers'), multimod_compile,
     ['T27013i.hs', '-v0 -hide-all-packages -package base -package containers -Wunused-packages'])
