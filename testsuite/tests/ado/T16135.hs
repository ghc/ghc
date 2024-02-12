{-# LANGUAGE ExistentialQuantification, ApplicativeDo #-}

{- This testcase failed before we treated Do statements via HsExpansions
   This test passes after #24406
-}

module Bug where

data T f = forall a. MkT (f a)

runf :: forall f. Functor f => IO (T f)
runf = do
    return ()
    MkT fa <- runf
    return $ MkT fa
