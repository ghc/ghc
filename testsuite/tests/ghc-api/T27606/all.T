# Replacing loaded code in the in-memory linker when the home unit graph has
# newer code, without a full unload.
# See Note [Automatically reloading stale linkables] in GHC.Linker.Loader
test('T27606a',
     [extra_run_opts(f'"{config.libdir}"'),
      extra_files(['B.hs']),
      req_rts_linker,
      when(arch('wasm32'), skip)],
     compile_and_run,
     ['-package ghc'])

test('T27606b',
     [extra_run_opts(f'"{config.libdir}"'),
      extra_files(['B.hs']),
      req_rts_linker,
      when(arch('wasm32'), skip)],
     compile_and_run,
     ['-package ghc'])

test('T27606c',
     [extra_run_opts(f'"{config.libdir}"'),
      extra_files(['B.hs']),
      req_rts_linker,
      when(arch('wasm32'), skip)],
     compile_and_run,
     ['-package ghc'])

test('T27606c_purge',
     [extra_run_opts(f'"{config.libdir}" purge'),
      extra_files(['T27606c.hs', 'B.hs']),
      req_rts_linker,
      when(config.ghc_dynamic, skip),
      when(arch('wasm32'), skip)],
     multimod_compile_and_run,
     ['T27606c', '-package ghc'])

test('T27606d',
     [extra_run_opts(f'"{config.libdir}"'),
      req_rts_linker,
      req_c,
      when(arch('wasm32'), skip)],
     compile_and_run,
     ['-package ghc'])

test('T27606f',
     [extra_run_opts(f'"{config.libdir}"'),
      req_rts_linker,
      when(arch('wasm32'), skip)],
     compile_and_run,
     ['-package ghc'])

test('T27606e',
     [extra_run_opts(f'"{config.libdir}"'),
      req_rts_linker,
      unless(config.ghc_dynamic, skip),
      unless(have_dynamic(), skip),
      when(opsys('mingw32'), skip),
      when(arch('wasm32'), skip)],
     compile_and_run,
     ['-package ghc'])
