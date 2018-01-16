-- Test that DuplicateRecordFields works with NamedFieldPuns and
-- RecordWildCards

{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns, RecordWildCards #-}

data S = MkS { foo :: Int }
  deriving Show
data T = MkT { foo :: Int }
  deriving Show

f MkS{foo} = MkT{foo}

g MkT{..} = MkS{..}

h e = let foo = 6 in e { foo } :: S

main = do print a
          print b
          print c
          print d
  where
    foo = 42

    a = MkS{foo}
    b = f a
    c = g b
    d = h c
