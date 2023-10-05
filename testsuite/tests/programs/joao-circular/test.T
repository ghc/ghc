test('joao-circular',
     [extra_files(['Data_Lazy.hs', 'Funcs_Lexer.hs', 'Funcs_Parser_Lazy.hs', 'LrcPrelude.hs', 'Main.hs', 'Visfun_Lazy.hs', 'inp']),
      when(fast(), skip),
      when(doing_ghci(), extra_hc_opts('-fobject-code')),
      # This can take a while to compile, especially with
      # LLVM backend (as is necessary on, e.g., ARM)
      compile_timeout_multiplier(3.0),
      extra_run_opts('inp 40')],
     multimod_compile_and_run,
     ['Main', '-O'])
