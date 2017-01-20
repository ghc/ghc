module T13156 where

f g x = let r :: [a] -> [a]
            r = case g x of True -> reverse . reverse
                            False -> reverse
        in
        r `seq` r `seq` True


{- Expected -ddump-prep looks like this.
   (Room for improvement on the case (case ..) line.)

-- RHS size: {terms: 9, types: 9, coercions: 0}
T13156.f1 :: forall a. [a] -> [a]
[GblId, Arity=1, Caf=NoCafRefs, Str=<S,1*U>, Unf=OtherCon []]
T13156.f1 =
  \ (@ a_aC4) (x_sNG [Occ=Once] :: [a]) ->
    case GHC.List.reverse @ a x_sNG of sat_sNH { __DEFAULT ->
    GHC.List.reverse1 @ a sat_sNH (GHC.Types.[] @ a)
    }

-- RHS size: {terms: 13, types: 20, coercions: 0}
T13156.f :: forall p. (p -> GHC.Types.Bool) -> p -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=<C(S),1*C1(U)><L,U>,
 Unf=OtherCon []]
T13156.f =
  \ (@ p_aBS)
    (g_sNI [Occ=Once!] :: p -> GHC.Types.Bool)
    (x_sNJ [Occ=Once] :: p) ->
    case case g_sNI x_sNJ of {
           GHC.Types.False -> GHC.List.reverse @ GHC.Types.Any;
           GHC.Types.True -> T13156.f1 @ GHC.Types.Any
         }
    of
    { __DEFAULT ->
    GHC.Types.True
    }

-}
