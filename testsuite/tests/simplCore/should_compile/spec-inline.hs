{- This test checks that specialiations can apply inside
   wrappers.  In particular, the wrapper for 'foo' should
   look like

         Tmpl= \ (n_aal [Occ=Once!] :: GHC.Types.Int) ->
                 case n_aal of _ { GHC.Types.I# ipv_smZ [Occ=Once] ->
                 case Roman.foo_$s$wgo ipv_smZ 6 of ww_spp { __DEFAULT ->
                 GHC.Types.I# ww_spp
                 }
                 }}]
   Note the $s$wgo.  That in turn allows $wgo to be dead code.
-}

module Roman where

foo :: Int -> Int
foo n = n `seq` go (Just n) (Just (6::Int))
  where
    go u (Just x)
      = x `seq`
        case u of
          Nothing -> go (Just 10) (Just m)
          Just n
                 | n <= 0    -> 0
                 | n < 100   -> go (Just (n-2)) (Just x)
                 | n < 500   -> go (Just (n-3)) (Just m)
                 | otherwise -> go (Just (n-1)) (Just (m+m))
      where
        m = x+x+x+x+x+x+x
