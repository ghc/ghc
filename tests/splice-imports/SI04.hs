{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI04 where

-- Importing `sid` twice at different levels works.
import SI01A
import splice SI01A

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
