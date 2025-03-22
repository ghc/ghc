{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Main  where
import Data.Foldable (Foldable(..))
import Data.Functor.Identity (Identity(..))

class (Foldable t) => Build t where
  polyFold :: forall a -> t a

instance Build Maybe where
  polyFold x = Nothing

instance Build [] where
  polyFold x = []

main :: IO ()
main = do
  print $ polyFold Int
  print $ 1

