
module Bugs.Bug2
       ( main
       ) where

import Test.HUnit hiding ( Test )
import Test.Framework
import Test.Framework.Providers.HUnit

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

main :: Test
main =
  testCase "Control Char Parsing (#2)" $
  parseString "\"test\\^Bstring\"" @?= "test\^Bstring"

 where
   parseString :: String -> String
   parseString input =
      case parse parser "Example" input of
        Left{} -> error "Parse failure"
        Right str -> str

   parser :: Parser String
   parser = P.stringLiteral $ P.makeTokenParser haskellDef