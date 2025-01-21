{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI03 where

import SI01A

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
