{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ApplicativeDo #-}

module T22326_fail_ado where

m :: IO ()
m = do
  type a <- undefined
  pure ()