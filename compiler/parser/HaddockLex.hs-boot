module HaddockLex ( Token(..), tokenise ) where

import RdrName

tokenise :: String -> [Token]

data Token
  = TokPara
  | TokNumber
  | TokBullet
  | TokDefStart
  | TokDefEnd
  | TokSpecial Char
  | TokIdent [RdrName]
  | TokString String
  | TokURL String
  | TokAName String
  | TokBirdTrack String
