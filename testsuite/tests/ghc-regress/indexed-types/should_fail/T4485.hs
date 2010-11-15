-- The behavior of type-inference and OverlappingInstances has changed
-- between GHC 6.12 and GHC 7.0 such that the following code
-- type-checks under 6.12, but not 7.0rc2. I assume this change has
-- something to do with the new type checker in GHC 7, but it is not
-- clear to me if this change in behavior is intended. Nor am I clear
-- how to achieve something similar to the old behavior. This is
-- preventing HSP (and by extension, happstack) from migrating to GHC
-- 7. I reported this earlier on the mailing lists, but I have further
-- simplied the test case here.

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
  , FlexibleContexts, FlexibleInstances, UndecidableInstances
  , TypeSynonymInstances, GeneralizedNewtypeDeriving
  , OverlappingInstances 
  #-}
module XMLGenerator where

newtype XMLGenT m a = XMLGenT (m a)
   deriving (Functor, Monad)

class Monad m => XMLGen m where
 type XML m
 data Child m
 genElement :: String -> XMLGenT m (XML m)

class XMLGen m => EmbedAsChild m c where
    asChild :: c -> XMLGenT m [Child m]

instance (EmbedAsChild m c, m1 ~ m) => EmbedAsChild m (XMLGenT m1 c)

instance (XMLGen m,  XML m ~ x) => EmbedAsChild m x

data Xml = Xml
data IdentityT m a = IdentityT (m a)
instance Monad (IdentityT m)
instance XMLGen (IdentityT m) where
    type XML (IdentityT m) = Xml

data Identity a = Identity a
instance Monad Identity

instance EmbedAsChild (IdentityT IO) (XMLGenT Identity ())

data FooBar = FooBar

instance EmbedAsChild (IdentityT IO) FooBar where
  asChild b = asChild $ (genElement "foo")
  -- asChild :: FooBar -> XMLGenT (XMLGenT (IdentityT IO) [Child (IdentitiyT IO)])

{-     ---------- Deriving the constraints ----------
 asChild :: EmbedAsChild m c => c -> XMLGenT m [Child m]
 genElement :: XMLGen m => String -> XMLGenT m (XML m)

 Wanted: EmbedAsChild m c, with m = IdentityT IO
                                c = XMLGenT meta (XML meta)
         XMLGen meta

  ie     EmbedAsChild (IdentityT IO) (XMLGen meta (XML meta)
         XMLGen meta

We have instances
    EmbedAsChild (IdentityT IO) FooBar
    EmbedAsChild (IdentityT IO) (XMLGenT Identity ())
    EmbedAsChild m              (XMLGenT m1 c)
    EmbedAsChild m              x
-}
