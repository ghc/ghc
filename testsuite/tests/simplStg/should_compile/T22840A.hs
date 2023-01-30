module T22840A where

data T = MkT !(Maybe Bool)

disp :: T -> String
disp (MkT b) =
  case b of
    Nothing -> "Nothing"
    Just _ -> "Just"
