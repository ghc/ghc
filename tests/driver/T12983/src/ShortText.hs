module ShortText where

data ShortText = ShortText String

toString :: ShortText -> String
toString (ShortText s) = s
