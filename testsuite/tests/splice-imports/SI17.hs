{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitStagePersistence #-}
module SI17 where

boo = [| \a -> $(const [| a |] 'a) |]
