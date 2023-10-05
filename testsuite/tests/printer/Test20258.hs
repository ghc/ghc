{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Test20258 where

x = 1

-- Comment
;

data Foo = Foo

-- After TyClD
;

instance Monoid CIRB where
  mempty = CIRB mempty mempty mempty mempty

-- After InstD
;

deriving instance Eq (GenTickish 'TickishPassCore)

-- After DerivD
;

transferCodingStr DeflateTransferCoding  = "deflate"

-- After ValD
;

getContentType :: Int

-- After SigD
;

type MyMaybe :: Type -> Type

-- After KindSigD
;

default (Integer)

-- After DefD
;

foreign import ccall unsafe "isDoubleFinite" isDoubleFinite :: Double -> Int

-- After ForD
;

{-# DEPRECATED foo2 [] #-}

-- After WarningD
;

{-# ANN module FromA #-}

-- After AnnD
;

{-# RULES "myrule2" id f = f #-}

-- After RuleD
;

$foo

-- After SpliceD
;

type role Representational representational

-- After RoleAnnotD
;

getContentType = 1

-- Note: skipping DocD, only generated in haddock mode
