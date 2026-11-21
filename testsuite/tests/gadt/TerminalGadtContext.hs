{-# LANGUAGE ExplicitNamespaces, FlexibleInstances, GADTs, NoExperimentalDependentTypes #-}
{-# LANGUAGE NoRequiredTypeArguments, TypeApplications #-}
module Main where

import TerminalGadtContextProvider (G(..), Observe(..), Recursive(..), TypeTag(..), IndexedTag(..))
import Type.Reflection (typeRep)

data Token = Token Bool deriving Eq

instance Show Token where
  show (Token b) = "client:" ++ show b

render :: G a b -> String
render (Store x (type b)) = show x ++ ":" ++ show (x == x)
render (Narrow n (type b)) = show n

narrow :: forall a. G a a -> Bool
narrow (Narrow n (type Int)) = n == 23
narrow (Store _ (type _)) = False

instance Observe (Recursive Int Bool) where
  observe _ = True

recursive :: Recursive a Char -> (a, Bool)
recursive (Recur x (type Char)) = (x, observe (Recur x (type Bool)))

readTypeTag :: TypeTag -> (Int, String)
readTypeTag (TypeTag n (type a)) = (n, show (typeRep @a))

readIndexedTag :: IndexedTag a -> (Int, String)
readIndexedTag (IndexedTag n (type a)) = (n, show (typeRep @a))

main :: IO ()
main = do
  putStrLn (render (Store @Token (Token True) (type Char)))
  print (narrow (Narrow @Int 23 (type Int)))
  print (recursive (Recur @Int 13 (type Char)))
  print (readTypeTag (TypeTag 19 (type Bool)), readIndexedTag (IndexedTag 23 (type Char)))
