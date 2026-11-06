test('ghcilink001',
     [extra_files(['TestLink.hs', 'f.c']),
      when(ghc_dynamic(), expect_fail), # dynamic ghci can't load '.a's
      unless(doing_ghci, skip),
      req_interp],                         # JS backend doesn't use ghci
     makefile_test, ['ghcilink001'])

test('ghcilink002', [extra_files(['TestLink.hs', 'f.c']),
                     when(unregisterised(), fragile(16085)),
                     unless(doing_ghci, skip),
                     unless(have_dynamic(), skip)],
     makefile_test, ['ghcilink002'])

test('ghcilink003',
     [ unless(doing_ghci, skip),
       # from Big Sur onwards, we can't dlopen libstdc++.dylib
       # anymore. Will produce:
       #   dlopen(libstdc++.dylib, 5): image not found
       when(opsys('darwin'), fragile(16083)),
       req_interp
     ], makefile_test, ['ghcilink003'])

test('ghcilink004',
     [extra_files(['TestLink.hs', 'f.c']),
      unless(doing_ghci, skip),
      when(unregisterised(), fragile(16085)),
      req_rts_linker],
     makefile_test, ['ghcilink004'])

test('ghcilink005',
     [extra_files(['TestLink.hs', 'f.c']),
      when(unregisterised(), fragile(16085)),
      unless(doing_ghci, skip),
      req_dynamic_lib_support,
      req_interp,
      when(opsys('linux') and not ghc_dynamic(), fragile(20706))],
     makefile_test, ['ghcilink005'])

test('ghcilink006',
     [ unless(doing_ghci, skip),
       # from Big Sur onwards, we can't dlopen libstdc++.dylib
       # anymore. Will produce:
       #   dlopen(libstdc++.dylib, 5): image not found
       when(opsys('darwin'), fragile(16083)),
       req_interp
     ], makefile_test, ['ghcilink006'])

test('T3333',
     [unless(doing_ghci, skip),
      when(unregisterised(), fragile(17018)),
      req_c],
     makefile_test, ['T3333'])

test('T11531',
     [extra_files(['T11531.hs', 'T11531.c', 'T11531.h']),
      unless(doing_ghci, skip),
      unless(opsys('linux'), skip),
      req_c,
      req_interp,
      fragile(11531)],
     makefile_test, ['T11531'])

test('T14708',
     [extra_files(['T14708.hs', 'add.c']),
      unless(doing_ghci, skip),
      unless(ghc_dynamic(), skip)],
     makefile_test, ['T14708'])

test('T15729',
     [extra_files(['T15729.hs', 'T15729.c']),
      unless(doing_ghci, skip),
      req_c],
     makefile_test, ['T15729'])

test('big-obj', [extra_files(['big-obj-c.c', 'big-obj.hs']),
                    unless(doing_ghci, skip), unless(opsys('mingw32'), skip)],
     makefile_test, ['big-obj'])

test('T25155', [req_c, req_th, req_interp, req_rts_linker, extra_files(['T25155_iserv_main.c', 'T25155_iserv.hs', 'T25155_TH.hs'])], makefile_test, [])

# unload_wkr-throw{,-ext}: a link failure in one module must not silently
# unload another module that was already linked.
#
# When resolveObjs fails, GHC.Linker.Loader.loadObjects calls unload_wkr, which
# empties objs_loaded/bcos_loaded/bco_loader_state and returns the new state.
# But every caller of unload_wkr except `unload` itself then throws on the
# Failed result, so modifyLoaderState's modifyMVar restores the *old* state and
# GHC goes on believing UnloadWkrA is loaded after the interpreter has dropped
# it.  Evaluating UnloadWkrA.hello then fails, and the :reload after it calls
# unloadObj on an object the RTS has already unloaded.
#
# Only reachable when the interpreter is not dynamic: the dynamic branch of
# loadObjects goes through dynLoadObjs and never calls resolveObjs at all.
# For the internal interpreter that means a non-dynamic GHC (hostIsDynamic is
# fixed at build time), hence the ghc_dynamic skip.  The external interpreter
# takes its way from the session, so -static is enough there.
#
# The expected output is the *fixed* behaviour: UnloadWkrB fails to link and
# nothing else does, so UnloadWkrA keeps working and :reload succeeds.
#
# FIXME: replace the expect_broken ticket numbers once the bug is filed.

def normalise_unload_wkr(s):
    import re
    # The failing object is named with the full path of the test directory.
    s = re.sub(r'\S*UnloadWkrB\.o', 'UnloadWkrB.o', s)
    # With -fexternal-interpreter the messages are prefixed with the name of
    # the iserv binary, which GHC generates into a temporary directory.
    # normalise_errmsg only knows how to normalise the compiler's own name.
    s = re.sub(r'^ghc_tmp_\d+\.iserv:', 'ghc:', s, flags=re.M)
    return s

test('unload_wkr-throw',
     [extra_files(['UnloadWkrA.hs', 'UnloadWkrB.hs']),
      extra_run_opts('-fobject-code'),
      only_ways(['ghci']),
      unless(doing_ghci, skip),
      req_rts_linker,
      when(ghc_dynamic(), skip),
      normalise_errmsg_fun(normalise_unload_wkr),
      expect_broken(28104)],
     ghci_script, ['unload_wkr-throw.script'])

test('unload_wkr-throw-ext',
     [extra_files(['UnloadWkrA.hs', 'UnloadWkrB.hs',
                   'unload_wkr-throw.script']),
      extra_run_opts('-static -fexternal-interpreter -fobject-code'),
      only_ways(['ghci']),
      unless(doing_ghci, skip),
      req_rts_linker,
      normalise_errmsg_fun(normalise_unload_wkr),
      expect_broken(28104)],
     ghci_script, ['unload_wkr-throw.script'])
