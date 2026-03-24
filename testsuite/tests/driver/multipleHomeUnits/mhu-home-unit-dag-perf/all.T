test('mhu-home-unit-dag-perf',
     [ collect_compiler_stats('bytes allocated', 2)
     , extra_files(['../gen-home-unit-dag.py'])
     , pre_cmd('python3 gen-home-unit-dag.py --units 300 --out-dir generated')
     , js_broken(22349)
     , when(arch('wasm32'), skip)
     , pre_cmd_timeout_multiplier(5)
     , compile_timeout_multiplier(5)
     ],
     multiunit_compile,
     [[f'generated/unit{i:04d}' for i in range(1, 301)], '-v0 -fhide-source-paths'])
