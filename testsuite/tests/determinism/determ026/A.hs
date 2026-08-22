{-# LANGUAGE TemplateHaskell #-}
module A where

-- reifyInstances at a bare type variable returns the unifying instances,
-- whose order must not depend on the order of Uniques (#27459).
--
-- The printed heads avoid pprint/show of anything containing a NameU:
-- those embed uniques and would differ between the two runs regardless
-- of instance order.

import Language.Haskell.TH

$(do let headName :: Type -> String
         headName (AppT f _) = headName f
         headName (ConT n)   = show n
         headName ListT      = "[]"
         headName (TupleT i) = "Tuple" ++ show i
         headName ArrowT     = "->"
         headName _          = "<other-type>"
         instHead (InstanceD _ _ (AppT _ arg) _) = headName arg
         instHead _                              = "<other-dec>"
     insts <- reifyInstances ''Show [VarT (mkName "a")]
     runIO (mapM_ (putStrLn . instHead) insts)
     return [])
