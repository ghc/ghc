
test('andy_cherry', [when(fast(), skip), extra_run_opts('.')],
     multimod_compile_and_run, ['Main', '-cpp'])
