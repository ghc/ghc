{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module T13782 where

import Language.Haskell.TH

$(do TyConI (DataD _ _ [KindedTV a1 _] _ _ _) <- reify ''Maybe
     [f,a2] <- mapM newName ["f","a"]
     return [ SigD f (ForallT [KindedTV a2 (AppT (ConT ''Maybe) (VarT a1))]
                   [] (ConT ''Int))
            , ValD (VarP f) (NormalB (LitE (IntegerL 42))) []
            ])
