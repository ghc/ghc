{-# LANGUAGE TemplateHaskell #-}
module T5968 where

data Bar a = Bar $( [t| a |] )


