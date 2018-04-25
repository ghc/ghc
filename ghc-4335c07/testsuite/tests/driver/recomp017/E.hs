{-# LANGUAGE TypeApplications #-}
module E where
import A
import B
import C
import D
c :: F (a, C) -> Bool
c = id
e :: () -> Bool
e = c . b @ C
