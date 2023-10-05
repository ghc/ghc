module Base where

data QName = QName
data Definition = D

udef :: a
udef = udef

getConstInfo :: Monad m => QName -> m Definition
getConstInfo = udef
