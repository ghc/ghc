{-# LANGUAGE TemplateHaskell #-}

-- This failed in 6.10.1, as the Name's for True and False in
-- Language.Haskell.TH.Syntax.{trueName,falseName} were wrong.

module TH_spliceE6 where

a = $( (\b -> [| b |]) True )
b = $( (\m -> [| m |]) (Just 'm') )
c = $( (\e -> [| e |]) (Left 'e' :: Either Char Bool) )

