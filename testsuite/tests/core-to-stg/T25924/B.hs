{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies, QuantifiedConstraints, TypeAbstractions #-}
module B where

import Data.Kind

class ABITypeable a where
  abiTypeInfo :: String
  abiTypeInfo = ""

  unused :: a -> a
  unused x = x

data REF a

instance ABITypeable () where
instance ABITypeable a => ABITypeable (REF a) where

class (ABITypeable a, ABITypeable a) => YulCatObj a where -- crash stops without duplicate constraint
instance YulCatObj ()
instance YulCatObj a => YulCatObj (REF a)

type YulO1 a = YulCatObj a
type YulO2 a b = (YulCatObj a, YulCatObj b)


type YulCat :: Type -> Type -> Type
data YulCat a b where
  YulExtendType :: forall b. (YulO2 () b) => YulCat () b
  YulComp :: forall a b c. YulCat c b -> YulCat a c -> YulCat a b
  YulJmpB :: forall a b. (YulO2 a b) =>  YulCat a b

data Trie  a b where
  Z :: Trie  a a
  (:.) :: (YulCatObj a, YulCatObj b) => YulCat a b -> Trie b c -> Trie a c

type Cat a b = forall c. Trie b c -> Trie a c

normalize :: forall a b unused  ξ. (Int ~ unused,  YulCatObj a, YulCatObj b)
  => Trie a b -> (forall c. YulCatObj c => Trie a c -> YulCat c b -> ξ) -> ξ
normalize t0 k = case t0 of
  Z -> k Z undefined
  φ :. f -> normalize f $ \f' s -> case f' of
                                     Z -> k Z (s `YulComp` φ)
                                     _ -> undefined


toSMC :: forall a b . (YulCatObj a, YulCatObj b) => Cat a b -> YulCat a b
toSMC t = normalize (t Z) $ \f g -> case f of
  Z -> g
  _ -> error "toSMC: normalisation process failed"


encode   :: (YulCatObj r, YulCatObj a, YulCatObj b) => (a `YulCat` b) -> (P r a -> P r b)
encode φ (Y f)    = Y (\x -> f (φ :. x))


type P :: Type -> Type -> Type
data P r a = Y (Cat r a)

fromP :: P r a -> Cat r a
fromP (Y f) = f


decode :: (YulCatObj a, YulCatObj b) => (P a a -> P a b) -> YulCat a b
decode f  = toSMC (extract f)

extract ::(YulCatObj a, YulCatObj b) => (P a a -> P a b) -> Cat a b
extract f = fromP (f (Y id))


yulShow :: YulCat a' b' -> String
yulShow (YulExtendType @b) = "Te" <> abiTypeInfo @b
yulShow (YulComp cb ac)    = yulShow ac <> yulShow cb
yulShow YulJmpB            = "Jb"


lfn' :: forall b unused.
  ( YulO1 (REF b)
  , () ~ unused
  ) =>
  (forall r. YulO1 r => P r () -> P r (REF b)) -> String
lfn' f = yulShow (decode f)


extendType'l :: forall a r. (YulO1 a, YulO1 r) => P r () -> P r a
extendType'l = encode YulExtendType

keccak256'l :: forall a r. YulO2 r a => P r a -> P r ()
keccak256'l = encode YulJmpB
