{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
module Main(main) where
import GHC.TypeLits

--------------------------------------------------------------------------------
-- Test top-reactions

tsub :: SingI x => Sing (x + y) -> Sing y -> Sing x
tsub _ _ = sing

tdiv :: SingI x => Sing (x * y) -> Sing y -> Sing x
tdiv _ _ = sing

troot :: SingI x => Sing (x ^ y) -> Sing y -> Sing x
troot _ _ = sing

tlog :: SingI y => Sing (x ^ y) -> Sing x -> Sing y
tlog _ _ = sing

tleq :: (SingI x, (x <=? y) ~ True) => Sing y -> Sing x
tleq _ = sing

main :: IO ()
main = print [ show (tsub  (sing :: Sing 5) (sing :: Sing 3)) == "2"
             , show (tdiv  (sing :: Sing 8) (sing :: Sing 2)) == "4"
             , show (troot (sing :: Sing 9) (sing :: Sing 2)) == "3"
             , show (tlog  (sing :: Sing 8) (sing :: Sing 2)) == "3"
             , show (tleq  (sing :: Sing 0))                  == "0"
             ]



