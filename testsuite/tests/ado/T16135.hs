{-# LANGUAGE ExistentialQuantification, ApplicativeDo #-}

module Bug where

data T f = forall a. MkT (f a)

runf :: forall f. Functor f => IO (T f)
runf = do
    return ()
    MkT fa <- runf
    return $ MkT fa
