{-# LANGUAGE Safe #-}
{-# LANGUAGE TemplateHaskell #-}
module SafeLang12_B ( Class(..), mkSimpleClass ) where

import Language.Haskell.TH

class Class a where
	c :: a

mkSimpleClass :: Name -> Q [Dec]
mkSimpleClass name = do
	TyConI (DataD [] dname [] cs _) <- reify name
	((NormalC conname []):_) <- return cs
	ClassI (ClassD [] cname [_] [] [SigD mname _]) _ <- reify ''Class
	return [InstanceD [] (AppT (ConT cname) (ConT dname)) [FunD mname
            [Clause [] (NormalB (ConE conname)) []]]]

