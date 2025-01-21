{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI01 where

-- Using a splice imported thing, inside an untyped and typed splice works
import splice SI01A

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
