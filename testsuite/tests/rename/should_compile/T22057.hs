{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS -Wall #-}
module Thing (thing) where

import Language.Haskell.TH

thing :: Q ()
thing = do
  name <- newName "x"
  -- warning:
  _ <- [| let ($(pure (VarP name)), _) = (3.0, 4.0) in $(pure (VarE name)) |]
  -- warning:
  _ <- [| let ($(pure (VarP name))   ) =  3.0       in $(pure (VarE name)) |]
  -- no warning:
  _ <- [| let  $(pure (VarP name))     =  3.0       in $(pure (VarE name)) |]
  return ()
