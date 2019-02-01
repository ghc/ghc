module B where

import A.Sig

exclaimA :: A -> String
exclaimA = (++ "!") . showA
