{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI03 where

import SI01A

-- You get an error message if you don't splice import `sid`.

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
