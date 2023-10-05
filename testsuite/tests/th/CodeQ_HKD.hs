{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module CodeQ_HKD where
import GHC.Exts
import Data.Kind
import Language.Haskell.TH hiding (Type)

data T (f :: forall r . (TYPE r) -> Type) = MkT (f Int) (f Int#)


tcodeq :: T CodeQ
tcodeq = MkT [||5||] [||5#||]
