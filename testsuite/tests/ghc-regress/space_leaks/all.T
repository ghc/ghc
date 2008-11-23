
test('space_leak_001',
     # Before trac #2747 was fixed this was 565, after it was 3
     space('peak_megabytes_allocated', 1, 5),
     compile_and_run_space,
     [''])
