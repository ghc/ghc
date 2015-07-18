test('qq008',
     [when(fast(), skip),
      extra_clean(['QQ.hi', 'QQ.o', 'Test.hi', 'Test.o']),
      pre_cmd('$MAKE -s --no-print-directory TH_QQ'),
      expect_broken(10181),
      ],
     multimod_compile,
     ['Test', '-v0'])
