test('mhu-perf',
     [ collect_compiler_stats('bytes allocated',2),
       extra_files(['genLargeHMU','Setup.hs']),
       pre_cmd('$MAKE -s --no-print-directory mhu-perf'),
       js_broken(22349),
       compile_timeout_multiplier(5)
     ],
     multiunit_compile,
     [['unitTop1', 'unitTop2'], '-fhide-source-paths'])
