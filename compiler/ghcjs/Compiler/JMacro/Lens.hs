
{-# LANGUAGE ScopedTypeVariables, GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeOperators #-}

module Compiler.JMacro.Lens where
import Prelude
-- import Control.Lens
import Compiler.JMacro.Base
import Control.Category hiding ((.), id)
-- import Data.Profunctor
-- import Data.Profunctor.Unsafe
import Data.Functor
import Data.Functor.Contravariant
import Control.Applicative
import Data.Map (Map)
import Data.Text (Text)
import Data.Foldable as Foldable
import Control.Monad.State        as State
import Control.Monad.Reader.Class as Reader
import Control.Monad.Identity
import qualified Data.Coerce as Coercible -- (Coercible, coerce)
import Data.Monoid
-- import Data.Distributive

import qualified Data.Proxy as X (Proxy (..))
import qualified Data.Typeable as X (typeRep, eqT)
import qualified Data.Type.Equality as X

import Data.Typeable
import Data.Data


class Default a where def :: a

-- bits and pieces from the lens library, until we have updated everything
-- to move away from it

newtype Folding f a = Folding { getFolding :: f a }
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
type ASetter s t a b = (a -> Identity b) -> s -> Identity t
type ASetter' s a = ASetter s s a a
type Setting p s t a b = p a (Identity b) -> s -> Identity t
type Setting' p s a = Setting p s s a a
type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t
type Setter' s a = Setter s s a a

type Prism s t a b = forall f. (Applicative f) => (a -> (f b)) -> (s -> (f t))
type Prism' s a = Prism s s a a
type Iso s t a b = forall f. (Functor f) => (a -> f b) -> (s -> f t)
type Iso' s a = Iso s s a a
type Fold a c = forall f. (Gettable f, Applicative f) => (c -> f c) -> a -> f a
class Functor f => Gettable f where
  -- | Replace the phantom type argument.
  coerce :: f a -> f b

instance Settable Identity where
  untainted = runIdentity
  {-# INLINE untainted #-}
  untaintedDot = (runIdentity #.)
  {-# INLINE untaintedDot #-}
  taintedDot = (Identity #.)
  {-# INLINE taintedDot #-}

class Profunctor p where
  -- | Map over both arguments at the same time.
  --
  -- @'dimap' f g ≡ 'lmap' f '.' 'rmap' g@
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  {-# INLINE dimap #-}

  -- | Map the first argument contravariantly.
  --
  -- @'lmap' f ≡ 'dimap' f 'id'@
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  {-# INLINE lmap #-}

  -- | Map the second argument covariantly.
  --
  -- @'rmap' ≡ 'dimap' 'id'@
  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id
  {-# INLINE rmap #-}

  -- | Strictly map the second argument argument
  -- covariantly with a function that is assumed
  -- operationally to be a cast, such as a newtype
  -- constructor.
  --
  -- /Note:/ This operation is explicitly /unsafe/
  -- since an implementation may choose to use
  -- 'unsafeCoerce' to implement this combinator
  -- and it has no way to validate that your function
  -- meets the requirements.
  --
  -- If you implement this combinator with
  -- 'unsafeCoerce', then you are taking upon yourself
  -- the obligation that you don't use GADT-like
  -- tricks to distinguish values.
  --
  -- If you import "Data.Profunctor.Unsafe" you are
  -- taking upon yourself the obligation that you
  -- will only call this with a first argument that is
  -- operationally identity.
  --
  -- The semantics of this function with respect to bottoms
  -- should match the default definition:
  --
  -- @('Profuctor.Unsafe.#.') ≡ \\_ -> \\p -> p \`seq\` 'rmap' 'coerce' p@
  (#.) :: forall a b c q. Coercible.Coercible c b => q b c -> p a b -> p a c
  (#.) = \_ -> \p -> p `seq` rmap (Coercible.coerce (id :: c -> c) :: b -> c) p
  {-# INLINE (#.) #-}

  -- | Strictly map the first argument argument
  -- contravariantly with a function that is assumed
  -- operationally to be a cast, such as a newtype
  -- constructor.
  --
  -- /Note:/ This operation is explicitly /unsafe/
  -- since an implementation may choose to use
  -- 'unsafeCoerce' to implement this combinator
  -- and it has no way to validate that your function
  -- meets the requirements.
  --
  -- If you implement this combinator with
  -- 'unsafeCoerce', then you are taking upon yourself
  -- the obligation that you don't use GADT-like
  -- tricks to distinguish values.
  --
  -- If you import "Data.Profunctor.Unsafe" you are
  -- taking upon yourself the obligation that you
  -- will only call this with a second argument that is
  -- operationally identity.
  --
  -- @('.#') ≡ \\p -> p \`seq\` \\f -> 'lmap' 'coerce' p@
  (.#) :: forall a b c q. Coercible.Coercible b a => p b c -> q a b -> p a c
  (.#) = \p -> p `seq` \_ -> lmap (Coercible.coerce (id :: b -> b) :: a -> b) p
  {-# INLINE (.#) #-}

  {-# MINIMAL dimap | (lmap, rmap) #-}

instance Profunctor (->) where
  dimap ab cd bc = cd . bc . ab
  {-# INLINE dimap #-}
  lmap = flip (.)
  {-# INLINE lmap #-}
  rmap = (.)
  {-# INLINE rmap #-}
  (#.) _ = Coercible.coerce (\x -> x :: b) :: forall a b. Coercible.Coercible b a => a -> b
  (.#) pbc _ = Coercible.coerce pbc
  {-# INLINE (#.) #-}
  {-# INLINE (.#) #-}

class Profunctor p => Choice p where
  -- | Laws:
  --
  -- @
  -- 'left'' ≡ 'dimap' swapE swapE '.' 'right'' where
  --   swapE :: 'Either' a b -> 'Either' b a
  --   swapE = 'either' 'Right' 'Left'
  -- 'rmap' 'Left' ≡ 'lmap' 'Left' '.' 'left''
  -- 'lmap' ('right' f) '.' 'left'' ≡ 'rmap' ('right' f) '.' 'left''
  -- 'left'' '.' 'left'' ≡ 'dimap' assocE unassocE '.' 'left'' where
  --   assocE :: 'Either' ('Either' a b) c -> 'Either' a ('Either' b c)
  --   assocE ('Left' ('Left' a)) = 'Left' a
  --   assocE ('Left' ('Right' b)) = 'Right' ('Left' b)
  --   assocE ('Right' c) = 'Right' ('Right' c)
  --   unassocE :: 'Either' a ('Either' b c) -> 'Either' ('Either' a b) c
  --   unassocE ('Left' a) = 'Left' ('Left' a)
  --   unassocE ('Right' ('Left' b)) = 'Left' ('Right' b)
  --   unassocE ('Right' ('Right' c)) = 'Right' c
  -- @
  left'  :: p a b -> p (Either a c) (Either b c)
  left' =  dimap (either Right Left) (either Right Left) . right'

  -- | Laws:
  --
  -- @
  -- 'right'' ≡ 'dimap' swapE swapE '.' 'left'' where
  --   swapE :: 'Either' a b -> 'Either' b a
  --   swapE = 'either' 'Right' 'Left'
  -- 'rmap' 'Right' ≡ 'lmap' 'Right' '.' 'right''
  -- 'lmap' ('left' f) '.' 'right'' ≡ 'rmap' ('left' f) '.' 'right''
  -- 'right'' '.' 'right'' ≡ 'dimap' unassocE assocE '.' 'right'' where
  --   assocE :: 'Either' ('Either' a b) c -> 'Either' a ('Either' b c)
  --   assocE ('Left' ('Left' a)) = 'Left' a
  --   assocE ('Left' ('Right' b)) = 'Right' ('Left' b)
  --   assocE ('Right' c) = 'Right' ('Right' c)
  --   unassocE :: 'Either' a ('Either' b c) -> 'Either' ('Either' a b) c
  --   unassocE ('Left' a) = 'Left' ('Left' a)
  --   unassocE ('Right' ('Left' b)) = 'Left' ('Right' b)
  --   unassocE ('Right' ('Right' c)) = 'Right' c
  -- @
  right' :: p a b -> p (Either c a) (Either c b)
  right' =  dimap (either Right Left) (either Right Left) . left'

  {-# MINIMAL left' | right' #-}

instance Choice (->) where
  left' ab (Left a) = Left (ab a)
  left' _ (Right c) = Right c
  {-# INLINE left' #-}
  right' = fmap
  {-# INLINE right' #-}


class (Applicative f, {-Distributive f,-} Traversable f) => Settable f where
  untainted :: f a -> a

  untaintedDot :: Profunctor p => p a (f b) -> p a b
  untaintedDot g = g `seq` rmap untainted g
  {-# INLINE untaintedDot #-}

  taintedDot :: Profunctor p => p a b -> p a (f b)
  taintedDot g = g `seq` rmap pure g
  {-# INLINE taintedDot #-}

rewriteOf :: ASetter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf l f = go where
  go = transformOf l (\x -> maybe x go (f x))
{-# INLINE rewriteOf #-}


universeOf :: Getting [a] a a -> a -> [a]
universeOf l = go where
  go a = a : foldMapOf l go a
{-# INLINE universeOf #-}

universeOnOf :: Getting [a] s a -> Getting [a] a a -> s -> [a]
universeOnOf b = foldMapOf b . universeOf
{-# INLINE universeOnOf #-}

transformOf :: ASetter a b a b -> (b -> b) -> a -> b
transformOf l f = go where
  go = f . over l go
{-# INLINE transformOf #-}

tinplate :: (Data s, Typeable a) => Traversal' s a
tinplate f = gfoldl (step f) pure
{-# INLINE tinplate #-}

step :: forall s a f r. (Applicative f, Typeable a, Data s) => (a -> f a) -> f (s -> r) -> s -> f r
step f w s = w <*> case X.eqT :: Maybe (s X.:~: a) of
  Just X.Refl -> f s
  Nothing   -> tinplate f s
{-# INLINE step #-}


use :: MonadState s m => Getting a s a -> m a
use l = State.gets (view l)
{-# INLINE use #-}

view :: MonadReader s m => Getting a s a -> m a
view l = Reader.asks (getConst #. l Const)
{-# INLINE view #-}

-- | The 'mempty' equivalent for a 'Gettable' 'Applicative' 'Functor'.
noEffect :: (Applicative f, Gettable f) => f a
noEffect = coerce $ pure ()
{-# INLINE noEffect #-}


instance (Gettable f, Applicative f) => Semigroup (Folding f a) where
  Folding fr <> Folding fs = Folding (fr *> fs)
  {-# INLINE (<>) #-}


instance (Gettable f, Applicative f) => Monoid (Folding f a) where
  mempty = Folding noEffect
  {-# INLINE mempty #-}
  Folding fr `mappend` Folding fs = Folding (fr *> fs)
  {-# INLINE mappend #-}

folded :: Foldable f => Fold (f c) c
folded f = coerce . getFolding . foldMap (Folding . f)

(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)

(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = State.modify (l .~ b)

(%=) :: MonadState s m => ASetter s s a b -> (a -> b) -> m ()
l %= f = State.modify (l %~ f)
{-# INLINE (%=) #-}

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

(?~) :: ASetter s t a (Maybe b) -> b -> s -> t
l ?~ b = set l (Just b)
{-# INLINE (?~) #-}

infixl 8 ^.
infix 4 %=, .=, +=, -=
infixr 4 %~, +~, -~, .~, ?~

(+=) :: (MonadState s m, Num a) => ASetter' s a -> a -> m ()
l += b = State.modify (l +~ b)
{-# INLINE (+=) #-}

(-=) :: (MonadState s m, Num a) => ASetter' s a -> a -> m ()
l -= b = State.modify (l -~ b)
{-# INLINE (-=) #-}

(+~) :: Num a => ASetter s t a a -> a -> s -> t
l +~ n = over l (+ n)
{-# INLINE (+~) #-}

(-~) :: Num a => ASetter s t a a -> a -> s -> t
l -~ n = over l (subtract n)
{-# INLINE (-~) #-}

(.~) :: ASetter s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity #. l (Identity #. f)
{-# INLINE over #-}

set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity #. l (\_ -> Identity b)
{-# INLINE set #-}


type Getting r s a = (a -> Const r a) -> s -> Const r s

instance Gettable (Const r) where
  coerce (Const m) = Const m

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where _1 :: Lens s t a b
class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where _2 :: Lens s t a b
class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where _3 :: Lens s t a b
class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where _4 :: Lens s t a b
class Field5 s t a b | s -> a, t -> b, s b -> t, t a -> s where _5 :: Lens s t a b


instance Field1 (a,b) (a',b) a a' where
  _1 k ~(a,b) = k a <&> \a' -> (a',b)
  {-# INLINE _1 #-}

instance Field1 (a,b,c) (a',b,c) a a' where
  _1 k ~(a,b,c) = k a <&> \a' -> (a',b,c)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d) (a',b,c,d) a a' where
  _1 k ~(a,b,c,d) = k a <&> \a' -> (a',b,c,d)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e) (a',b,c,d,e) a a' where
  _1 k ~(a,b,c,d,e) = k a <&> \a' -> (a',b,c,d,e)
  {-# INLINE _1 #-}

instance Field1 (a,b,c,d,e,f) (a',b,c,d,e,f) a a' where
  _1 k ~(a,b,c,d,e,f) = k a <&> \a' -> (a',b,c,d,e,f)
  {-# INLINE _1 #-}

instance Field2 (a,b) (a,b') b b' where
  _2 k ~(a,b) = k b <&> \b' -> (a,b')
  {-# INLINE _2 #-}

instance Field2 (a,b,c) (a,b',c) b b' where
  _2 k ~(a,b,c) = k b <&> \b' -> (a,b',c)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d) (a,b',c,d) b b' where
  _2 k ~(a,b,c,d) = k b <&> \b' -> (a,b',c,d)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e) (a,b',c,d,e) b b' where
  _2 k ~(a,b,c,d,e) = k b <&> \b' -> (a,b',c,d,e)
  {-# INLINE _2 #-}

instance Field2 (a,b,c,d,e,f) (a,b',c,d,e,f) b b' where
  _2 k ~(a,b,c,d,e,f) = k b <&> \b' -> (a,b',c,d,e,f)
  {-# INLINE _2 #-}

instance Field3 (a,b,c) (a,b,c') c c' where
  _3 k ~(a,b,c) = k c <&> \c' -> (a,b,c')
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d) (a,b,c',d) c c' where
  _3 k ~(a,b,c,d) = k c <&> \c' -> (a,b,c',d)
  {-# INLINE _3 #-}

instance Field3 (a,b,c,d,e) (a,b,c',d,e) c c' where
  _3 k ~(a,b,c,d,e) = k c <&> \c' -> (a,b,c',d,e)
  {-# INLINE _3 #-}

instance Field4 (a,b,c,d) (a,b,c,d') d d' where
  _4 k ~(a,b,c,d) = k d <&> \d' -> (a,b,c,d')
  {-# INLINE _4 #-}

instance Field4 (a,b,c,d,e) (a,b,c,d',e) d d' where
  _4 k ~(a,b,c,d,e) = k d <&> \d' -> (a,b,c,d',e)
  {-# INLINE _4 #-}

instance Field5 (a,b,c,d,e) (a,b,c,d,e') e e' where
  _5 k ~(a,b,c,d,e) = k e <&> \e' -> (a,b,c,d,e')
  {-# INLINE _5 #-}

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap
{-# INLINE mapped #-}

sets :: ((c -> d) -> a -> b) -> Setter a b c d
sets f g = pure . f (untainted . g)
{-# INLINE sets #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

class Category k => Isomorphic k where
  -- | Build this morphism out of an isomorphism
  --
  -- The intention is that by using 'isomorphic', you can supply both halves of an
  -- isomorphism, but k can be instantiated to @(->)@, so you can freely use
  -- the resulting isomorphism as a function.
  isomorphic :: (a -> b) -> (b -> a) -> k a b

  -- | Map a morphism in the target category using an isomorphism between morphisms
  -- in Hask.
  isomap :: ((a -> b) -> c -> d) -> ((b -> a) -> d -> c) -> k a b -> k c d

to k = dimap k (contramap k)

(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. l = toListOf l s
{-# INLINE (^..) #-}

infixl 8 ^..

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []
{-# INLINE toListOf #-}

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)
{-# INLINE foldrOf #-}

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst #. l (Const #. f)
{-# INLINE foldMapOf #-}

type Traversal' s a = Traversal s s a a
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-------------------

_DeclStat :: Prism' JStat Ident
_DeclStat = prism DeclStat
                  (\x -> case x of DeclStat y -> Right y
                                   _          -> Left x)

_ReturnStat :: Prism' JStat JExpr
_ReturnStat = prism ReturnStat
                    (\x -> case x of ReturnStat y -> Right y
                                     _            -> Left x)

_IfStat :: Prism' JStat (JExpr, JStat, JStat)
_IfStat = prism (\(x1, x2, x3) -> IfStat x1 x2 x3)
                (\x -> case x of IfStat y1 y2 y3 -> Right (y1, y2, y3)
                                 _               -> Left x)

_WhileStat :: Prism' JStat (Bool, JExpr, JStat)
_WhileStat = prism (\(x1, x2, x3) -> WhileStat x1 x2 x3)
                   (\x -> case x of WhileStat y1 y2 y3 -> Right (y1, y2, y3)
                                    _                  -> Left x)

_ForInStat :: Prism' JStat (Bool, Ident, JExpr, JStat)
_ForInStat =
  prism (\(x1, x2, x3, x4) -> ForInStat x1 x2 x3 x4)
        (\x -> case x of ForInStat y1 y2 y3 y4 -> Right (y1, y2, y3, y4)
                         _                     -> Left x)

_SwitchStat :: Prism' JStat (JExpr, [(JExpr, JStat)], JStat)
_SwitchStat =
  prism (\(x1, x2, x3) -> SwitchStat x1 x2 x3)
        (\x -> case x of SwitchStat y1 y2 y3 -> Right (y1, y2, y3)
                         _                   -> Left x)

_TryStat :: Prism' JStat (JStat, Ident, JStat, JStat)
_TryStat =
  prism (\(x1, x2, x3, x4) -> TryStat x1 x2 x3 x4)
        (\x -> case x of TryStat y1 y2 y3 y4 -> Right (y1, y2, y3, y4)
                         _                   -> Left x)

_BlockStat :: Prism' JStat [JStat]
_BlockStat =
  prism BlockStat
        (\x -> case x of BlockStat y -> Right y
                         _           -> Left x)

_ApplStat :: Prism' JStat (JExpr, [JExpr])
_ApplStat =
  prism (uncurry ApplStat)
        (\x -> case x of ApplStat y1 y2 -> Right (y1, y2)
                         _              -> Left x)

_UOpStat :: Prism' JStat (JUOp, JExpr)
_UOpStat =
  prism (uncurry UOpStat)
        (\x  -> case x of UOpStat y1 y2 -> Right (y1, y2)
                          _             -> Left x)

_AssignStat :: Prism' JStat (JExpr, JExpr)
_AssignStat =
  prism (uncurry AssignStat)
        (\x -> case x of AssignStat y1 y2 -> Right (y1, y2)
                         _                -> Left x)

_UnsatBlock :: Prism' JStat (IdentSupply JStat)
_UnsatBlock =
  prism UnsatBlock
        (\x -> case x of UnsatBlock y -> Right y
                         _            -> Left x)

_LabelStat :: Prism' JStat (JsLabel, JStat)
_LabelStat =
  prism (uncurry LabelStat)
        (\x -> case x of LabelStat y1 y2 -> Right (y1, y2)
                         _               -> Left x)

_BreakStat :: Prism' JStat (Maybe JsLabel)
_BreakStat =
  prism BreakStat
        (\x -> case x of BreakStat y -> Right y
                         _           -> Left x)

_ContinueStat :: Prism' JStat (Maybe JsLabel)
_ContinueStat =
  prism ContinueStat
        (\x -> case x of ContinueStat y -> Right y
                         _              -> Left x)

-- JExpr

_ValExpr :: Prism' JExpr JVal
_ValExpr =
  prism ValExpr
        (\x -> case x of ValExpr y -> Right y
                         _         -> Left x)

_SelExpr :: Prism' JExpr (JExpr, Ident)
_SelExpr =
  prism (uncurry SelExpr)
        (\x -> case x of SelExpr y1 y2 -> Right (y1, y2)
                         _             -> Left x)

_IdxExpr :: Prism' JExpr (JExpr, JExpr)
_IdxExpr =
  prism (uncurry IdxExpr)
        (\x -> case x of IdxExpr y1 y2 -> Right (y1, y2)
                         _             -> Left x)

_InfixExpr :: Prism' JExpr (JOp, JExpr, JExpr)
_InfixExpr =
  prism (\(x1, x2, x3) -> InfixExpr x1 x2 x3)
        (\x -> case x of InfixExpr y1 y2 y3 -> Right (y1, y2, y3)
                         _                  -> Left x)

_UOpExpr :: Prism' JExpr (JUOp, JExpr)
_UOpExpr =
  prism (uncurry UOpExpr)
        (\x -> case x of UOpExpr y1 y2 -> Right (y1, y2)
                         _             -> Left x)

_IfExpr :: Prism' JExpr (JExpr, JExpr, JExpr)
_IfExpr =
  prism (\(x1, x2, x3) -> IfExpr x1 x2 x3)
        (\x -> case x of IfExpr y1 y2 y3 -> Right (y1, y2, y3)
                         _               -> Left x)

_ApplExpr :: Prism' JExpr (JExpr, [JExpr])
_ApplExpr =
  prism (uncurry ApplExpr)
        (\x -> case x of ApplExpr y1 y2 -> Right (y1, y2)
                         _              -> Left x)

_UnsatExpr :: Prism' JExpr (IdentSupply JExpr)
_UnsatExpr
  = prism UnsatExpr
          (\x -> case x of UnsatExpr y -> Right y
                           _           -> Left x)

-- JVal

_JVar :: Prism' JVal Ident
_JVar =
  prism JVar
        (\x -> case x of JVar y -> Right y
                         _      -> Left x)
_JList :: Prism' JVal [JExpr]
_JList =
  prism JList
        ( \x -> case x of JList y -> Right y
                          _       -> Left x)

_JDouble :: Prism' JVal SaneDouble
_JDouble =
  prism JDouble
        (\x -> case x of JDouble y -> Right y
                         _         -> Left x)

_JInt :: Prism' JVal Integer
_JInt =
  prism JInt
        (\x -> case x of JInt y -> Right y
                         _      -> Left x)

_JStr :: Prism' JVal Text
_JStr =
  prism JStr
        (\x -> case x of JStr y -> Right y
                         _      -> Left x)

_JRegEx :: Prism' JVal Text
_JRegEx
  = prism JRegEx
          (\x -> case x of JRegEx y -> Right y
                           _        -> Left x)

_JHash :: Prism' JVal (Map Text JExpr)
_JHash =
  prism JHash
        (\x -> case x of JHash y -> Right y
                         _       -> Left x)

_JFunc :: Prism' JVal ([Ident], JStat)
_JFunc =
  prism (uncurry JFunc)
        (\x -> case x of JFunc y1 y2 -> Right (y1, y2)
                         _           -> Left x)

_UnsatVal :: Prism' JVal (IdentSupply JVal)
_UnsatVal =
  prism UnsatVal
        (\x -> case x of UnsatVal y -> Right y
                         _          -> Left x)

-- Ident

_TxtI :: Iso' Ident Text
_TxtI = iso (\(TxtI x) -> x) TxtI
