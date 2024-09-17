test('unboxedsums_unit_tests',
     [ only_ways(['normal']), extra_run_opts('"' + config.libdir + '"') ],
     compile_and_run,
     ['-package ghc'])

test('unarise',       normal, compile_and_run, [''])
test('unboxedsums1',  normal, compile_and_run, [''])
test('unboxedsums2',  normal, compile_and_run, [''])
test('unboxedsums3',  normal, compile_and_run, [''])
test('unboxedsums4',  normal, compile_fail, [''])
test('unboxedsums5',  normal, compile, [''])
test('unboxedsums6',  normal, compile_and_run, [''])
test('unboxedsums7',  normal, compile_and_run, [''])
test('unboxedsums8',  normal, compile_and_run, [''])
test('unboxedsums9',  normal, compile_and_run, [''])
test('unboxedsums10', normal, compile_and_run, [''])
test('unboxedsums11', normal, compile_and_run, [''])
test('unboxedsums12', normal, compile, [''])

test('UnboxedSumsTH', [req_th], compile, [''])
test('UnboxedSumsTH_Fail', [req_th], compile_fail, [''])

test('ffi1', normal, compile_fail, [''])
test('thunk', only_ways(['normal']), compile_and_run, [''])
test('T12375', only_ways(['normal']), compile_and_run, [''])
test('empty_sum', only_ways(['normal']), compile_and_run, [''])
test('sum_rr', normal, compile, [''])
test('T12711', normal, ghci_script, ['T12711.script'])

test('UbxSumLevPoly', normal, compile, ['-Wno-overlapping-patterns'])
test('T14051', normal, multi_compile, ['T14051.hs', [('T14051a.hs', '')], '-O2 -v0'])
test('T19645', normal, compile_and_run, [''])
test('T20858', normal, ghci_script, ['T20858.script'])
test('T20858b', [extra_files(['T20858.hs'])
                ,extra_hc_opts("-fprint-explicit-runtime-reps -fprint-explicit-kinds")]
              , ghci_script, ['T20858b.script'])
test('T20859', normal, compile, [''])

test('unpack_sums_1', normal, compile_and_run, ['-O'])
test('unpack_sums_2', normal, compile, ['-O'])
test('unpack_sums_3', normal, compile_and_run, ['-O'])
test('unpack_sums_4', normal, compile_and_run, ['-O'])
test('unpack_sums_5', normal, compile, [''])
test('unpack_sums_6', [fragile(22504), js_broken(22374)], compile_and_run, ['-O'])
test('unpack_sums_7', normal, makefile_test, [])
test('unpack_sums_8', normal, compile_and_run, [""])
test('unpack_sums_9', normal, compile, [""])

# TODO: Need to run this in --slow mode only
# test('sum_api_annots',
#      [only_ways(['normal']),
#       extra_files([ "unboxedsums" + str(i) + ".hs" for i in range(1, 12) ])],
#      makefile_test, [])
test('T22208', normal, compile, ['-dstg-lint -dcmm-lint'])
test('ManyUbxSums',
     [ pre_cmd('{compiler} --run ./GenManyUbxSums.hs'),
       extra_files(['GenManyUbxSums.hs', 'ManyUbxSums_Addr.hs']),
       req_interp,
       omit_ghci,
     ],
     multi_compile_and_run,
     ['ManyUbxSums',
        [('ManyUbxSums_Addr.hs','')]
        , '-v0 -dstg-lint -dcmm-lint'])
