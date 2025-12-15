{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module T11342b where

type X = $( [t| 'x' :: Char |] )
