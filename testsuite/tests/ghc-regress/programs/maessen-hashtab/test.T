
test('maessen_hashtab',
     composes([reqlib('QuickCheck'),
               skip_if_fast,
               if_compiler_lt('ghc', '6.9', skip),
               extra_run_opts('99999')]),
     multimod_compile_and_run, ['HashTest', '-cpp'])
clean_o_hi()

