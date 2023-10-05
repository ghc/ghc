{-# LANGUAGE TemplateHaskell #-}
module T5597 where
import T5597a( f )

g = $(f [t| (Int, Int) |])
