
test('maessen_hashtab', composes([reqlib('QuickCheck'),
                                  skip_if_fast,
                                  extra_run_opts('99999'),
                                  expect_broken(1546)]),
     multimod_compile_and_run, ['HashTest', '-cpp'])
clean_o_hi()

