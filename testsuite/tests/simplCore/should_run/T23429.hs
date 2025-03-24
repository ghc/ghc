{-# OPTIONS_GHC -fno-specialise-incoherents #-}
{-# LANGUAGE MonoLocalBinds #-}
class C a where
  op :: a -> String

instance {-# OVERLAPPABLE #-} C a where
  op _ = "C a"
  {-# NOINLINE op #-}

instance {-# INCOHERENT #-} C (Maybe a) where
  op _ = "C (Maybe a)"
  {-# NOINLINE op #-}

instance {-# INCOHERENT #-} C (Maybe ()) where
  op _ = "C (Maybe ())"
  {-# NOINLINE op #-}

-- | Inhibit inlining, but keep specialize-ability
large :: a -> a
large x = x
{-# NOINLINE large #-}

bar :: C a => a -> String
bar x = large (large (large (large (large (large (large (large (large (large (large (large (large (large (op x))))))))))))))

gen :: a -> String -- No C a constraint, has to choose the incoherent generic instance
gen = bar

specMaybe :: Maybe a -> String -- C () constraint is resolved to the specialized instance for Maybe a
specMaybe = bar

specMaybeUnit :: Maybe () -> String -- C () constraint is resolved to the specialized instance for Maybe ()
specMaybeUnit = bar

main :: IO ()
main = do
  putStrLn $ "gen () == " <> gen (Just ())
  putStrLn $ "specMaybe () == " <> specMaybe (Just ())
  putStrLn $ "specMaybeUnit () == " <> specMaybeUnit (Just ())
