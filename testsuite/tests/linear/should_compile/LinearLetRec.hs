module NameCache where

data Name = Name
data NameCache = NameCache !Int !Name

extendOrigNameCache :: Name -> Name -> Name
extendOrigNameCache _ _ = Name

initNameCache :: Int -> [Name] -> NameCache
initNameCache us names
  = NameCache us (foldl extendOrigNameCache Name names)
