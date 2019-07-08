test('T16670_unboxed', [extra_files(['T16670_unboxed.hs']),
    req_interp, omit_ways(prof_ways), unless(have_dynamic(), skip)],
        makefile_test, ['T16670_unboxed'])

test('T16670_th', [extra_files(['T16670_th.hs', 'TH.hs']),
    req_interp, omit_ways(prof_ways), unless(have_dynamic(), skip)],
        makefile_test, ['T16670_th'])
