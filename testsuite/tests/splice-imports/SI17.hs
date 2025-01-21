{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoPathCSP #-}
module SI17 where

boo = [| \a -> $(const [| a |] 'a) |]
