{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.Haskell.TH

infixr 7 :***:
data GADT a where
  Prefix  :: Int -> Int -> GADT Int
  (:***:) :: Int -> Int -> GADT Int

$(do gadtName   <- newName "GADT2"
     prefixName <- newName "Prefix2"
     infixName  <- newName ":****:"
     a          <- newName "a"
     return [ DataD [] gadtName [KindedTV a () StarT] Nothing
              [ GadtC [prefixName]
                [ (Bang NoSourceUnpackedness NoSourceStrictness,ConT ''Int)
                , (Bang NoSourceUnpackedness NoSourceStrictness,ConT ''Int)
                ] (AppT (ConT gadtName) (ConT ''Int))
              , GadtC [infixName]
                [ (Bang NoSourceUnpackedness NoSourceStrictness,ConT ''Int)
                , (Bang NoSourceUnpackedness NoSourceStrictness,ConT ''Int)
                ] (AppT (ConT gadtName) (ConT ''Int))
              ] []
            , InfixD (Fixity 7 InfixR) infixName
            ])

$(return [])

deriving instance Show (GADT2 a)

main :: IO ()
main = do
  -- Verify that infix GADT constructors reify correctly
  putStrLn $(reify ''GADT   >>= stringE . pprint)
  putStrLn $(reify '(:***:) >>= stringE . pprint)
  -- Verify that reifyFixity returns something with (:***:)
  -- (but not with Prefix, since it has no fixity declaration)
  putStrLn $(reifyFixity 'Prefix  >>= stringE . show)
  putStrLn $(reifyFixity '(:***:) >>= stringE . show)
  -- Verify that spliced-in GADT infix constructors are actually infix
  print (1 :****: 4)
