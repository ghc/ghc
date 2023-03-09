{-# LANGUAGE
    GADTSyntax
  , RankNTypes
  , RebindableSyntax
#-}

import Prelude hiding ((>>=))

data InfDo where
    InfDo :: String -> (forall a. a -> InfDo) -> InfDo

prog :: InfDo
prog = do
    _ <- show (42 :: Int)
    prog
  where
    (>>=) = InfDo

main :: IO ()
main = let x = prog in x `seq` return ()
