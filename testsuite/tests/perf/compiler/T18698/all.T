test('T18698a',
     [collect_compiler_residency(15),
      collect_compiler_stats('bytes allocated', 1),
      extra_files(['T18698.hs'])
      ],
     multimod_compile,
     ['T18698', '-O2 -v0'])

test('T18698b',
     [collect_compiler_residency(15),
      collect_compiler_stats('bytes allocated', 1),
      extra_files(['T18698.hs'])
      ],
     multimod_compile,
     ['T18698', '-O2 -v0 -DCOERCE'])
