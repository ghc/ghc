
test('space_leak_001',
     # Before trac #2747 was fixed this was 565, after it was 3
     [stats_num_field('peak_megabytes_allocated', 3, 3),
      stats_num_field('max_bytes_used', 430000,
                                        450000),
                      # expected value: 440224
      stats_num_field('bytes allocated', 9340000000,
                                         9350000000)],
                       # expected value: 9343740368
     compile_and_run,
     [''])
