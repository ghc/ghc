{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI07 where

-- --make mode, -fno-code, we need object code for SI05A but not for SI07A
import SI07A
import splice SI05A

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
