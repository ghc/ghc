# Test family overlap testing performance by generating two modules defining
# numerous type instances.

test('T19703',
     [collect_compiler_stats('bytes allocated', 1),
      extra_files(['genT19703.py', 'Makefile']),
      pre_cmd('$MAKE -s --no-print-directory prep-T19703')
     ],
     compile,
     [''])