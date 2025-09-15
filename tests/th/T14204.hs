{-# LANGUAGE TemplateHaskell #-}
module T14204 where

import GHC.StaticPtr
import Language.Haskell.TH

main :: IO ()
main = putStrLn (deRefStaticPtr $(pure (StaticE (LitE (StringL "wat")))))
