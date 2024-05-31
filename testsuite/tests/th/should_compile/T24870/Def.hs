{-# LANGUAGE TemplateHaskell #-}

module SDef where

{-# NOINLINE aValue #-}
aValue = True

{-# NOINLINE aStrictFunction #-}
aStrictFunction !x = [| x |]
