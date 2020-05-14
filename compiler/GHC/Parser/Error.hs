module GHC.Parser.Error where

import GHC.Utils.Error
import Prelude
-- import Text.Show (Show(..))

data ParseError = ParseError ErrMsg

instance Show ParseError where
  show (ParseError e) = "ParseError => " ++ show e

parseErrorMsg :: ParseError -> ErrMsg
parseErrorMsg (ParseError e) = e
