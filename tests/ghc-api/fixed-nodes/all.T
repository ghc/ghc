test('FixedNodes',
     [extra_run_opts(f'"{config.libdir}"'),
     extra_files(['T1A.hs', 'T1B.hs', 'T1C.hs']),
     wasm_broken(25953),
     ],
     compile_and_run,
     ['-package ghc'])

test('ModuleGraphInvariants',
     [extra_run_opts(f'"{config.libdir}"'),
     extra_files(['T1A.hs', 'T1B.hs', 'T1C.hs']),
     ],
     compile_and_run,
     ['-package ghc'])

test('InterfaceModuleGraph',
     [extra_run_opts(f'"{config.libdir}"'),
     extra_files(['T1A.hs', 'T1B.hs', 'T1C.hs']),
     wasm_broken(25953)
     ],
     compile_and_run,
     ['-package ghc'])
