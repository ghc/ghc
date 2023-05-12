{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedDatatypes #-}
import GHC.Exts

type D1 :: TYPE (BoxedRep Unlifted)
data D1 = MkD1 !Int

showD1 :: D1 -> String
showD1 (MkD1 i) = "MkD1 " ++ show i

type D2 :: TYPE (BoxedRep Lifted)
data D2 = MkD2 !Int deriving stock Show

risky :: forall {r} (a :: TYPE (BoxedRep Unlifted)) (b :: TYPE r). a -> b
risky = unsafeCoerce#
{-# NOINLINE risky #-}

main :: IO ()
main = do
  putStrLn (showD1 (unsafeCoerce# (MkD1 11))) -- foo11
  print (unsafeCoerce# (MkD1 12) :: D2)       -- foo12
  putStrLn (showD1 (risky (MkD1 11)))         -- bar11
  print (risky (MkD1 12) :: D2)               -- bar12
