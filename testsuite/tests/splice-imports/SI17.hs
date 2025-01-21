{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoImplicitStagePersistence #-}
module SI17 where

-- Testing a level correct program is accepted (variable quotes and expression quotes)
boo = [| \a -> $(const [| a |] 'a) |]
