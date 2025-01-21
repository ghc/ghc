{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI04 where

-- Importing 'sid' from different places is ambiguous (even if it's not level ambiguous)
import SI01A
import splice SI05A

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
