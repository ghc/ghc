module T26548 where

newtype N a = MkN (Maybe a)
data T = MkT !(N Int) !(N Bool)

f x = case x of { MkT a b ->
      case x of { MkT c d -> MkT a d  } }
