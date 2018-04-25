test('joao-circular',
     [extra_files(['Data_Lazy.hs', 'Funcs_Lexer.hs', 'Funcs_Parser_Lazy.hs', 'LrcPrelude.hs', 'Main.hs', 'Visfun_Lazy.hs', 'inp']),
      when(fast(), skip),
      when(doing_ghci(), extra_hc_opts('-fobject-code')),
      extra_clean(['Data_Lazy.hi',         'Data_Lazy.o',
                   'Funcs_Lexer.hi',       'Funcs_Lexer.o',
                   'Funcs_Parser_Lazy.hi', 'Funcs_Parser_Lazy.o',
                   'LrcPrelude.hi',        'LrcPrelude.o',
                   'Main.hi',              'Main.o',
                   'Visfun_Lazy.hi',       'Visfun_Lazy.o']),
      # This can take a while to compile, especially with
      # LLVM backend (as is necessary on, e.g., ARM)
      compile_timeout_multiplier(3.0),
      extra_run_opts('inp 40')],
     multimod_compile_and_run,
     ['Main', '-O'])
