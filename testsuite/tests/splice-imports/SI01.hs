{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI01 where

import splice SI01A

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
