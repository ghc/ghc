
test('maessen_hashtab',
     [reqlib('QuickCheck'),
      extra_clean(['HashTest.hi', 'HashTest.o',
                   'Data/HashTab.hi', 'Data/HashTab.o']),
      skip_if_fast,
      extra_run_opts('99999')],
     multimod_compile_and_run, ['HashTest', '-cpp'])

