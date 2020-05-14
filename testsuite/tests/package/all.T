hide_all = '-hide-all-packages -XNoImplicitPrelude '
incr_containers = '-package "containers (Data.Map as Map, Data.Set)" '
inc_containers = '-package containers '
incr_ghc = '-package "ghc (GHC.Hs.Type as GHC.Hs.MyTypes, GHC.Hs.Utils)" '
inc_ghc = '-package ghc '
hide_ghc = '-hide-package ghc '

test('package01',  normal, compile,      [hide_all + incr_containers])
test('package01e', normalise_version('containers'), compile_fail, [hide_all + incr_containers])
test('package02',  normal, compile,      [hide_all + inc_containers + incr_containers])
test('package03',  normal, compile,      [hide_all + incr_containers + inc_containers])
test('package04',  normal, compile,      [incr_containers])
test('package05',  normal, compile,      [incr_ghc + inc_ghc])
test('package06',  normal, compile,      [incr_ghc])
test('package06e', normalise_version('ghc'), compile_fail, [incr_ghc])
test('package07e', normalise_version('ghc'), compile_fail, [incr_ghc + inc_ghc + hide_ghc])
test('package08e', normalise_version('ghc'), compile_fail, [incr_ghc + hide_ghc])
test('package09e', normal, compile_fail, ['-package "containers (Data.Map as M, Data.Set as M)"'])
test('package10',  normal, compile,      ['-hide-all-packages -package "ghc (GHC.Types.Unique.FM as Prelude)" '])

test('T4806', normalise_version('containers'), compile_fail, ['-ignore-package containers'])
test('T4806a', normalise_version('containers'), compile_fail, ['-ignore-package deepseq'])
