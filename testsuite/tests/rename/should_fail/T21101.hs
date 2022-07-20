{-# LANGUAGE RecordWildCards #-}
module T21101 where

data D = D Int Bool

f :: D -> ()
f D{..} = ()
