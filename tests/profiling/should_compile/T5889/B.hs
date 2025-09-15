{-# OPTIONS_GHC -fprof-auto #-}
module B where

plus_noinline :: Integer -> Integer -> Integer
plus_noinline x y = x + y
{-# NOINLINE plus_noinline #-}

-- | This is the key function. We do not want this to be inlined into bar, but
-- we DO want it to be inlined into main (in A.hs). Moreover, when it is inlined
-- into main, we don't want the values inside the tuple to be inlined. To
-- achieve this, in main we call bar with Nothing allowing split to be inlined
-- with the first case, where the values in tuple are calls to NOINLINE
-- functions.
split :: Integer -> Maybe Integer -> (Integer, Integer)
split n Nothing = (n `plus_noinline` 1, n `plus_noinline` 2)
split n (Just m) =
  if n == 0 then (m, m) else split (n - 1) (Just m)


{- | The simplified core for bar is:

[GblId,
 Arity=2,
 Str=<L,U><S,1*U>,
 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=2,unsat_ok=True,boring_ok=False)
         Tmpl= \ (n_a1Gq [Occ=OnceL] :: Integer)
                 (m_a1Gr [Occ=OnceL] :: Maybe Integer) ->
                 scc<bar>
                 let {
                   ds_s2rg :: (Integer, Integer)
                   [LclId]
                   ds_s2rg = scc<bar.(...)> split n_a1Gq m_a1Gr } in
                 plus_noinline
                   (scc<bar.y>
                    case ds_s2rg of { (y_a2ps [Occ=Once], _ [Occ=Dead])
                      -> y_a2ps })
                   (scc<bar.z>
                    case ds_s2rg of { (_ [Occ=Dead], z_a2pu [Occ=Once])
                      -> z_a2pu })}]
bar
  = \ (n_a1Gq :: Integer) (m_a1Gr :: Maybe Integer) ->
      scc<bar>
      case scc<bar.(...)> split n_a1Gq m_a1Gr of
      { (ww1_s2s7, ww2_s2s8) ->
      plus_noinline ww1_s2s7 ww2_s2s8
      }

Note that there are sccs around the (x,y) pattern match in the unfolding, but
not in the simplified function. See #5889 for a discussion on why the sccs are
present in one but not the other, and whether this is correct.

split is not inlined here, because it is a recursive function.

In A.hs, bar is called with m = Nothing, allowing split to be inlined (as it is
not recursive in that case) and the sccs ARE present in the simplified core of
main (as they are around function calls, not ids). This triggers the linker
error.

-}
bar :: Integer -> Maybe Integer -> Integer
bar n m = y `plus_noinline` z
  where
    (y, z) = split n m
