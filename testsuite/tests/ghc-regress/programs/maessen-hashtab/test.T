
test('maessen_hashtab',
     [reqlib('QuickCheck'),
      extra_clean(['HashTest.hi', 'HashTest.o',
                   'Data/HashTab.hi', 'Data/HashTab.o']),
      skip_if_fast,
      if_compiler_lt('ghc', '6.9', skip),
      extra_run_opts('99999')],
     multimod_compile_and_run, ['HashTest', '-cpp'])

