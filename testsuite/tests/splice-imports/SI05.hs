{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI04 where

import SI01A
import splice SI05A

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
