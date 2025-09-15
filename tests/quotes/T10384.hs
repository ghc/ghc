{-# LANGUAGE TemplateHaskellQuotes, RankNTypes, ScopedTypeVariables #-}
module A where
x = \(y :: forall a. a -> a) -> [|| y ||]
