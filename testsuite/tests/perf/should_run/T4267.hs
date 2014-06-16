data Tree a = Leaf | Node a !(Tree a) !(Tree a)

{-

This should produce a walker with unboxed integers.

Bad:

Rec {
go_r1us
  :: GHC.Types.Int -> Main.Tree GHC.Types.Int -> GHC.Types.Int
[GblId, Arity=2, Caf=NoCafRefs, Str=DmdType SS]
go_r1us =
  \ (z_aeS :: GHC.Types.Int) (ds_dmD :: Main.Tree GHC.Types.Int) ->
    case ds_dmD of _ {
      Main.Leaf -> z_aeS;
      Main.Node a1_aeU l_aeV r_aeW ->
        case go_r1us z_aeS l_aeV of _ { GHC.Types.I# ipv_snn ->
        case a1_aeU of _ { GHC.Types.I# y_anh ->
        go_r1us (GHC.Types.I# (GHC.Prim.+# ipv_snn y_anh)) r_aeW
        }
        }
    }
end Rec }


Good:

Rec {
$wgo_r2fS
  :: GHC.Prim.Int# -> Main.Tree GHC.Types.Int -> GHC.Prim.Int#
[GblId, Arity=2, Caf=NoCafRefs, Str=DmdType <L,U><S,1*U>]
$wgo_r2fS =
  \ (ww_s2eZ :: GHC.Prim.Int#) (w_s2eW :: Main.Tree GHC.Types.Int) ->
    case w_s2eW of _ [Occ=Dead] {
      Main.Leaf -> ww_s2eZ;
      Main.Node a1_aqv l_aqw r_aqx ->
        case $wgo_r2fS ww_s2eZ l_aqw of ww1_s2f3 { __DEFAULT ->
        case a1_aqv of _ [Occ=Dead] { GHC.Types.I# y_aTz ->
        $wgo_r2fS (GHC.Prim.+# ww1_s2f3 y_aTz) r_aqx
        }
        }
    }
end Rec }

-}

-- Strict, pre-order fold.
fold' :: (a -> b -> a) -> a -> Tree b -> a
fold' f = go
  where
    go z Leaf = z
    go z (Node a l r) = let z'  = go z l
                            z'' = f z' a
                        in z' `seq` z'' `seq` go z'' r


sumTree :: Int -> Tree Int -> Int
sumTree = fold' (+)


tree = Node 0 (Node 0 Leaf Leaf) (Node 0 Leaf Leaf)

main = sum [sumTree n tree | n <- [0..1000]] `seq` return ()
