
test('maessen_hashtab',
     [reqlib('QuickCheck'),
      extra_clean(['HashTest.hi', 'HashTest.o',
                   'Data/HashTab.hi', 'Data/HashTab.o']),
      when(fast(), skip),
# this test runs out of time when not optimised:
      omit_ways(['normal','ghci','threaded1']),
      extra_run_opts('99999')],
     multimod_compile_and_run, ['HashTest', '-cpp'])

