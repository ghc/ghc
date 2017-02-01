module T13156 where

f g x = let r :: [a] -> [a]
            r = case g x of True -> reverse . reverse
                            False -> reverse
        in
        r `seq` r `seq` r


{- Expected -ddump-prep looks like this.
   (Case-of-type-lambda an oddity of Core Prep.)

-- RHS size: {terms: 9, types: 9, coercions: 0, joins: 0/0}
T13156.f1 :: forall a. [a] -> [a]
[GblId, Arity=1, Caf=NoCafRefs, Str=<S,1*U>, Unf=OtherCon []]
T13156.f1 =
  \ (@ a) (x [Occ=Once] :: [a]) ->
    case GHC.List.reverse @ a x of sat { __DEFAULT ->
    GHC.List.reverse1 @ a sat (GHC.Types.[] @ a)
    }

-- RHS size: {terms: 17, types: 28, coercions: 0, joins: 0/0}
T13156.f
  :: forall p.
     (p -> GHC.Types.Bool) -> p -> [GHC.Types.Int] -> [GHC.Types.Int]
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=<C(S),1*C1(U)><L,U>,
 Unf=OtherCon []]
T13156.f =
  \ (@ p)
    (g [Occ=Once!] :: p -> GHC.Types.Bool)
    (x [Occ=Once] :: p) ->
    case \ (@ a) ->
           case g x of {
             GHC.Types.False -> GHC.List.reverse @ a;
             GHC.Types.True -> T13156.f1 @ a
           }
    of r [Dmd=<S,U>]
    { __DEFAULT ->
    case r @ GHC.Types.Any of { __DEFAULT -> r @ GHC.Types.Int }
    }

-}
