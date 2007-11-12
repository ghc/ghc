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
  | TokPic String
  | TokEmphasis String
  | TokAName String
  | TokBirdTrack String
