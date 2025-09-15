{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O2 #-}
module THBug where

data A
data B

concat <$> mapM (\_ -> (pure []))
  [ ''A
  , ''B
  ]
