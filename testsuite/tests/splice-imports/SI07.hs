{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI07 where

import SI07A
import splice SI05A

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
