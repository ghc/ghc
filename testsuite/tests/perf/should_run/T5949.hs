import Data.List

{-
This should get a worker that takes three arguments.

bad:

Rec {
$we_r1z8
  :: (GHC.Types.Int, GHC.Types.Int)
     -> GHC.Prim.Int# -> (# GHC.Types.Int, GHC.Types.Int #)
[GblId, Arity=2, Caf=NoCafRefs, Str=DmdType S(AA)L]
$we_r1z8 =
  \ (w_s1yf :: (GHC.Types.Int, GHC.Types.Int))
    (ww_s1yi :: GHC.Prim.Int#) ->
    case GHC.Prim.># ww_s1yi 10 of _ {
      GHC.Types.False -> $we_r1z8 w_s1yf (GHC.Prim.+# ww_s1yi 1);
      GHC.Types.True ->
        case w_s1yf of _ { (ww2_s1yp, ww3_s1yq) ->
        (# ww2_s1yp, ww3_s1yq #)
        }
    }
end Rec }

Good:

Rec {
$we_r2qK
  :: GHC.Types.Int
     -> GHC.Types.Int
     -> GHC.Prim.Int#
     -> (# GHC.Types.Int, GHC.Types.Int #)
[GblId, Arity=3, Caf=NoCafRefs, Str=DmdType <L,U><L,U><L,U>]
$we_r2qK =
  \ (ww_s2pS :: GHC.Types.Int)
    (ww1_s2pT :: GHC.Types.Int)
    (ww2_s2pX :: GHC.Prim.Int#) ->
    case GHC.Prim.tagToEnum# @ GHC.Types.Bool (GHC.Prim.># ww2_s2pX 10)
    of _ [Occ=Dead] {
      GHC.Types.False ->
        $we_r2qK ww_s2pS ww1_s2pT (GHC.Prim.+# ww2_s2pX 1);
      GHC.Types.True -> (# ww_s2pS, ww1_s2pT #)
    }
end Rec }

-}



e :: (Int, Int) -> Int -> (Int, Int)
e x y = x `seq` if y > 10
        then x
        else e x (y + 1)


main = foldr (seq) 0 [e (n,0) 0| n <- [0..10000]] `seq` return ()
