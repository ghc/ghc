{-# LANGUAGE TemplateHaskell #-}
module T22807B where
import T22807A

$(pure xs)
