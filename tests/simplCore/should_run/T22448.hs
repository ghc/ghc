{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-specialise-incoherents #-}
class C a where
  op :: a -> String

instance {-# OVERLAPPABLE #-} C a where
  op _ = "C a"
  {-# NOINLINE op #-}

instance {-# INCOHERENT #-} C () where
  op _ = "C ()"
  {-# NOINLINE op #-}

-- | Inhibit inlining, but keep specialize-ability
large :: a -> a
large x = x
{-# NOINLINE large #-}

bar :: C a => a -> String
bar x = large (large (large (large (large (large (large (large (large (large (large (large (large (large (op x))))))))))))))

spec :: () -> String -- C () constraint is resolved to the specialized instance
spec = bar

gen :: a -> String -- No C a constraint, has to choose the incoherent generic instance
gen = bar

main :: IO ()
main = do
  putStrLn $ "spec () == " <> spec ()
  putStrLn $ "gen () == " <> gen ()
