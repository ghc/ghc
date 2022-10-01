module NameCache where

data Name = Name
data NameCache = NameCache !Int !Name

extendOrigNameCache :: Name -> Name -> Name
extendOrigNameCache _ _ = Name

initNameCache :: Int -> [Name] -> NameCache
initNameCache us names
  = NameCache us (go Name names)
  where
    -- go will become a join point once $WNameCache inlines. That join point
    -- has a nullary exit join point with a problematic linearity.
    -- The NOINLINE makes sure that call-site loopification doesn't turn go into
    -- a joinrec before $WNameCache inlines
    go acc [] = acc
    -- head names `seq` ... so that `go` doesn't float to top-level
    go acc (n:ns) = head names `seq` go (extendOrigNameCache acc n) ns
    {-# NOINLINE go #-} -- see above comment
