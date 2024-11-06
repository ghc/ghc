test('T25284',
  [js_skip, # allocation counters aren't available on the JS backend
   extra_files(['Main.hs', 'A.hs', 'B.hs', 'Cls.hs']),
   extra_run_opts('+RTS -T -RTS')],
  multimod_compile_and_run,
  ['Main', ''])
