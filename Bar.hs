{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
module Bar where
import Language.Haskell.TH

foo :: Code Q (forall a. Show a => a -> String)
foo = [|| show ||]
