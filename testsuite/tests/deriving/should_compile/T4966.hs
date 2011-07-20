{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module HTk.Toolkit.TreeList (getObjectFromTreeList) where

class Eq c => CItem c

-- A bizarre instance decl!
-- People who use instance decls like this are asking for trouble
instance GUIObject w => Eq w where
  w1 == w2 = toGUIObject w1 == toGUIObject w2

data StateEntry a 
  = StateEntry (TreeListObject a) a -- Comment this 'a' out and it type checks
  deriving Eq

-- The delicate point about this test is that we want to 
-- infer a derived instance decl like this:
--    instance (CItem a, Eq a) => Eq (StateEntry a)
-- But note the instance decl for (Eq w) for any w!
-- There's a danger than we'll use that instance decl
-- to get the derived instance
--    instance (CItem a, GUIObject a) => Eq (StateEntry a)
-- And then that doesn't work subsequently

getObjectFromTreeList :: CItem a => StateEntry a -> Bool  
getObjectFromTreeList state = state == state

data CItem a => TreeListObject a

instance CItem a => Eq (TreeListObject a)

class GUIObject w where
  toGUIObject     :: w -> GUIOBJECT


data GUIOBJECT

instance Eq GUIOBJECT where
  (==) = undefined
  (/=) = undefined
