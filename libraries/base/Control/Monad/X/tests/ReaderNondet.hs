import Control.Monad.X.ReaderT
import Control.Monad.X.NondetT
import Control.Monad.X.Identity

t0,t1,t2 :: (MonadReader String m, MonadNondet m) => m String
t0        = local ('a':) mzero
t1        = (local ('a':) mzero) `mplus` ask
t2        = local ('a':) (mzero `mplus` ask)


run1 m   = runIdentity $ runReader "x" $ runNondet $ m
run2 m   = runIdentity $ runNondet $ runReader "x" $ m

test      :: Eq a => (forall m. (MonadReader String m, MonadNondet m) => m a) -> Maybe a -> Bool
test t r  = run1 t == r && run2 t == r


main    = do print $ test t0 Nothing
             print $ test t1 (Just "x")
             print $ test t2 (Just "ax")

