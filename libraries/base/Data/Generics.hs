-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Data types for generic definitions (GHC only).
--
-----------------------------------------------------------------------------

module Data.Generics ( 
	-- * Data types for the sum-of-products type encoding
	(:*:)(..), (:+:)(..), Unit(..),

	-- * Typeable and types-save cast
	Typeable(..),  cast, sameType, 

	-- * The Data class and related types
	Data( gmapT, gmapQ, gmapM, 
	      gfoldl, gfoldr, gunfold,
	      conOf, consOf ),
	Constr(..), 

	-- * Transformations (T), queries (Q), monadic transformations (Q), 
	--   and twin transformations (TT)
	GenericT, GenericQ, GenericM,
	mkT,  mkQ,  mkM, 
	extT, extQ, extM,
	mkTT,


	-- * Traversal combinators
	everything, something, everywhere, everywhereBut,
	synthesize, branches, undefineds,

	-- * Generic operations: equality, zip, read, show
	geq, gzip, gshow, gread,

	-- * Miscellaneous
	match, tick, count, alike	


 ) where

import Prelude	-- So that 'make depend' works

#ifdef __GLASGOW_HASKELL__
import GHC.Base ( (:*:)(..), (:+:)(..), Unit(..) )
#endif

import Data.Dynamic
import Control.Monad



---------------------------------------------
--
--	Operations involving Typeable only
--
---------------------------------------------

-- | Apply a function if appropriate or preserve term
mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f = case cast f of
               Just g -> g
               Nothing -> id

-- | Apply a function if appropriate or return a constant
mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
(r `mkQ` br) a = case cast a of
                    Just b  -> br b
                    Nothing -> r



-- | Apply a monadic transformation if appropriate; resort to return otherwise
mkM :: (Typeable a, Typeable b, Typeable (m a), Typeable (m b), Monad m)
    => (b -> m b) -> a -> m a
mkM f = case cast f of
          Just g  -> g
          Nothing -> return

-- | Extend a transformation
extT :: (Typeable a, Typeable b) => (a -> a) -> (b -> b) -> a -> a
extT f g = case cast g of
              Just g' -> g'
              Nothing -> f

-- | Extend a query
extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
extQ f g a = case cast a of
                Just b -> g b
                Nothing -> f a

-- | Extend a monadic transformation
extM :: (Typeable a, Typeable b, Typeable (m a), Typeable (m b), Monad m)
       => (a -> m a) -> (b -> m b) -> a -> m a
extM f g = case cast g of
              Just g' -> g'
              Nothing -> f

-- | Test two entities to be of the same type
sameType :: (Typeable a, Typeable b) => a -> b -> Bool
sameType (_::a) = False `mkQ` (\(_::a) -> True)



-- | Make a twin transformation
-- Note: Should be worked on 
mkTT :: (Typeable a, Typeable b, Typeable c)
     => (a -> a -> a)
     -> b -> c -> Maybe c
mkTT (f::a ->a->a) x y =
  case (cast x,cast y) of
    (Just (x'::a),Just (y'::a)) -> cast (f x' y')
    _ -> Nothing



---------------------------------------------
--
--	The Data class and its operations
--
---------------------------------------------

-- A class for traversal

class Typeable a => Data a where
  gmapT   :: (forall b. Data b => b -> b) -> a -> a
  gmapQ   :: (forall a. Data a => a -> u) -> a -> [u]
  gmapM   :: Monad m => (forall a. Data a => a -> m a) -> a -> m a

  gfoldl  :: (forall a b. Data a => c (a -> b) -> a -> c b)
          -> (forall g. g -> c g)
          -> a -> c a

  gfoldr  :: (forall a b. Data a => a -> c (a -> b) -> c b)
          -> (forall g. g -> c g)
          -> a -> c a


  -- | Find the constructor
  conOf   :: a -> Constr
  -- | Does not look at a; Could live in Typeable as well maybe
  consOf  :: a -> [Constr]

  gunfold :: (forall a b. Data a => c (a -> b) -> c b)
          -> (forall g. g -> c g)
          -> c a
          -> Constr
          -> c a

  -- No default method for gfoldl, gunfold, conOf, consOf

  -- Default methods for gfoldr, gmapT, gmapQ, gmapM, 
  -- in terms of gfoldl

  gfoldr f z = gfoldl (flip f) z

  gmapT f x = unID (gfoldl k ID x)
    where
      k (ID c) x = ID (c (f x))

  gmapQ f x = unQ (gfoldl k (const (Q id)) x) []
    where
      k (Q c) x = Q (\rs -> c (f x : rs))

  gmapM f = gfoldl k return
          where
            k c x = do c' <- c
                       x' <- f x
                       return (c' x')


  -- Default definition for gfoldl copes with basic datatypes
  gfoldl _ z = z


{-
 A variation for gmapQ using an ordinary constant type constructor.
 A problem is here that the associativety might be wrong.

  newtype Phantom x y = Phantom x
  runPhantom (Phantom x) = x

  gmapQ f = runPhantom . gfoldl f' z
   where
    f' r a = Phantom (f a : runPhantom r)
    z  = const (Phantom [])
-}
 

-- | Describes a constructor
data Constr = Constr { conString :: String }		-- Will be extended

-- | Instructive type synonyms
type GenericT = forall a. Data a => a -> a
type GenericQ r = forall a. Data a => a -> r
type GenericM m = forall a. Data a => a -> m a


-- Auxiliary type constructors for the default methods (not exported)
newtype ID x = ID { unID :: x }
newtype Q r a = Q { unQ  :: [r]->[r] }
newtype TQ r a = TQ { unTQ :: ([r]->[r],[GenericQ' r]) }

-- A twin variation on gmapQ
-- Note: Nested GenericQ (GenericQ ...) buggy in GHC 5.04

tmapQ :: forall r.
         (forall a b. (Data a, Data b) => a -> b -> r)
      -> (forall a b. (Data a, Data b) => a -> b -> [r])

tmapQ g x y = fst (unTQ (gfoldl k z y)) []
    where
      k (TQ (c,l)) x = TQ (\rs -> c (unQ' (head l) x:rs), tail l)
      z _            = TQ (id,gmapQ (\x -> Q' (g x)) x)

-- A first-class polymorphic version of GenericQ

data GenericQ' u = Q' { unQ' :: forall a. Data a => a -> u }



-- A first-class polymorphic version of GenericM

data Monad m => GenericM' m = M' { unM' :: forall a. Data a => a -> m a }

-- A type constructor for monadic twin transformations
newtype TM m a = TM { unTM :: (m a,[GenericM' m]) }

-- A twin variation on gmapM

tmapM :: forall m. Monad m
      => (forall a b. (Data a, Data b) => a -> b -> m b)
      -> (forall a b. (Data a, Data b) => a -> b -> m b)
tmapM g x y = fst (unTM (gfoldl k z y))
  where
    k (TM (f,l)) x = TM (f >>= \f' -> unM' (head l) x >>= return . f',tail l)
    z f            = TM (return f,gmapQ (\x -> M' (g x)) x)

---------------------------------------------
--
--	Combinators for data structure traversal
--
---------------------------------------------

-- | Summarise all nodes in top-down, left-to-right
everything :: Data a
           => (r -> r -> r)
           -> (forall a. Data a => a -> r)
           -> a -> r
everything k f x 
     = foldl k (f x) (gmapQ (everything k f) x)



-- | Look up something by means of a recognizer
something :: (forall a. Data a => a -> Maybe u)
          -> (forall a. Data a => a -> Maybe u)
something = everything orElse



-- | Left-biased choice
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                Just _  -> x
                Nothing -> y



-- | Some people like folding over the first maybe instead
x `orElse'` y = maybe y Just x



-- | Bottom-up synthesis of a data structure
synthesize :: (forall a. Data a => a -> s -> s)
           -> (s -> s -> s)
           -> s
           -> (forall a. Data a => a -> s)
synthesize f o z x = f x (foldr o z (gmapQ (synthesize f o z) x))



-- | Apply a transformation everywhere in bottom-up manner
everywhere :: (forall a. Data a => a -> a)
           -> (forall a. Data a => a -> a)
everywhere f = f . gmapT (everywhere f)



-- | Variation with stop condition
everywhereBut :: GenericQ Bool 
              -> GenericT -> GenericT
everywhereBut q f x
    | q x       = x
    | otherwise = f (gmapT (everywhereBut q f) x)



-- | Monadic variation
everywhereM :: (Monad m, Data a)
            => (forall b. Data b => b -> m b)
            -> a -> m a
everywhereM f x = do x' <- gmapM (everywhereM f) x
                     f x'


-- | Count immediate subterms
branches :: Data a => a -> Int
branches = length . gmapQ (const ())


-- |  Construct term with undefined subterms
undefineds :: Data a => Constr -> Maybe a
undefineds i =  gunfold (maybe Nothing (\x -> Just (x undefined)))
                        Just
                        Nothing
                        i


---------------------------------------------
--
--	Generic equality, zip, read, show
--
---------------------------------------------

-- | Generic equality
geq :: forall a. Data a => a -> a -> Bool
geq x y = geq' x y
 where
  geq' :: forall a b. (Data a, Data b) => a -> b -> Bool
  geq' x y = and ( (conString (conOf x) == conString (conOf y)) : tmapQ geq' x y)



-- | Generic zip
gzip :: (forall a b. (Data a, Data b) => a -> b -> Maybe b)
     -> (forall a b. (Data a, Data b) => a -> b -> Maybe b)
gzip f x y = 
  f x y
  `orElse`
  if conString (conOf x) == conString (conOf y)
   then tmapM (gzip f) x y
   else Nothing


-- Generic show
gshow :: Data a => a -> String
gshow t = "(" ++ conString (conOf t) ++ concat (gmapQ ((++) " ". gshow) t) ++ ")"



-- The type constructor for unfold a la ReadS from the Prelude
newtype GRead i a = GRead (i -> Maybe (a, i))
unGRead (GRead x) = x



-- Generic read
gread :: Data a => String -> Maybe (a, String)
gread s
 = do s' <- return $ dropWhile ((==) ' ') s
      guard (not (s' == ""))
      guard (head s' == '(')
      (c,s'')  <- breakConOf (dropWhile ((==) ' ') (tail s'))
      (a,s''') <- unGRead (gunfold f z e c) s''
      guard (not (s''' == "")) 
      guard (head s''' == ')')
      return (a,tail s''')
 where
  f cab = GRead (\s -> do (ab,s') <- unGRead cab s
                          (a,s'')  <- gread s'
                          return (ab a,s''))
  z c = GRead (\s -> Just (c,s))
  e   = GRead (const Nothing)


-- Get Constr at front
breakConOf :: String -> Maybe (Constr, String)

-- Assume an infix operators in parantheses
breakConOf ('(':s)
 = case break ((==) ')') s of
     (s'@(_:_),(')':s'')) -> Just (Constr ("(" ++ s' ++ ")"), s'')
     _ -> Nothing

-- Special treatment of multiple token constructors
breakConOf ('[':']':s) = Just (Constr "[]",s)

-- Try lex for ordinary constructor and basic datatypes
breakConOf s
 = case lex s of
     [(s'@(_:_),s'')] -> Just (Constr s',s'')
     _ -> Nothing



---------------------------------------------
--
--	Instances of the Data class
--
---------------------------------------------

instance Data Float where
 conOf x = Constr (show x)
 consOf _ = []
 gunfold f z e c = z (read (conString c))

instance Data Char where
 conOf x = Constr (show x)
 consOf _ = []
 gunfold f z e c = z (read (conString c))

{-	 overlap
instance Data String where
 conOf x = Constr (show x)
 consOf _ = []
 gunfold f z e = z . read

-}

instance Data Bool where
 conOf False = Constr "False"
 conOf True  = Constr "True"
 consOf _    = [Constr "False",Constr "True"]
 gunfold f z e (Constr "False") = z False
 gunfold f z e (Constr "True")  = z True
 gunfold _ _ e _       = e

instance Data a => Data [a] where
  gmapT  f   []     = []
  gmapT  f   (x:xs) = (f x:f xs)
  gmapQ  f   []     = []
  gmapQ  f   (x:xs) = [f x,f xs]
  gmapM  f   []     = return []
  gmapM  f   (x:xs) = f x >>= \x' -> f xs >>= \xs' -> return (x':xs')
  gfoldl f z []     = z []
  gfoldl f z (x:xs) = z (:) `f` x `f` xs
  gfoldr f z []     = z []
  gfoldr f z (x:xs) = f xs (f x (z (:)))
  conOf [] = Constr "[]"
  conOf (_:_) = Constr "(:)"
  gunfold f z e (Constr "[]")  = z []
  gunfold f z e (Constr "(:)") = f (f (z (:)))
  gunfold _ _ e _     = e
  consOf _ = [Constr "[]",Constr "(:)"]




{- ----------------------------------------------------
	Comments illustrating generic instances

   An illustrative instance for a nested datatype
   
   data Nest a = Box a | Wrap (Nest [a])
    
    nestTc = mkTyCon "Nest"
    
    instance Typeable a => Typeable (Nest a) where
      typeOf n = mkAppTy nestTc [typeOf (paratype n)]
       where
	paratype :: Nest a -> a
	paratype _ = undefined
   
   instance (Data a, Data [a]) => Data (Nest a) where
    gmapT f (Box a)  = Box (f a)
    gmapT f (Wrap w) = Wrap (f w)
    gmapQ f (Box a)  = [f a]
    gmapQ f (Wrap w) = [f w]
    gmapM f (Box a)  = f a >>= return . Box
    gmapM f (Wrap w) = f w >>= return . Wrap
    conOf (Box _) = "Box"
    conOf (Wrap _) = "Wrap"
    consOf _ = ["Box","Wrap"]
    gunfold f z e "Box"  = f (z Box)
    gunfold f z e "Wrap" = f (z Wrap)
    gunfold _ _ e _      = e
   
   
   
   -- An illustrative instance for local quantors
   
   instance Data GenericT' where
    gmapT f (T' g) = (T' (f g))
    conOf _ = "T'"
    consOf _ = ["T'"]
   
   
   -- test code only
   instance Typeable GenericT' where
    typeOf _ = undefined
   
   
   
   -- The instance for function types
   -- needs -fallow-undecidable-instances

instance Typeable (a -> b) => Data (a -> b) where
 gmapT f = id
 gmapQ f = const []
 gmapM f = return
 conOf _ = "->"
 consOf _ = ["->"]
-}


--------------------------------------------------------
-- A first-class polymorphic version of GenericT
-- Note: needed because [GenericT] not valid in GHC 5.04

{-	Comment out for now (SLPJ 17 Apr 03)

data GenericT' = T' (forall a. Data a => a -> a)
unT' (T' x) = x

-- A type constructor for twin transformations

newtype IDL r a = IDL (a,[GenericT'])
unIDL (IDL x) = x



-- A twin variation on gmapT

tmapT :: (forall a b. (Data a, Data b) => a -> b -> b)
      -> (forall a b. (Data a, Data b) => a -> b -> b)
tmapT g x y = fst (unIDL (gfoldl k z y))
  where
    k (IDL (f,l)) x = IDL (f (unT' (head l) x),tail l)
    z f             = IDL (f,gmapQ (\x -> T' (g x)) x)



-- A first-class polymorphic version of GenericQ

data GenericQ' u = Q' (forall a. Data a => a -> u)
unQ' (Q' x) = x




-}





-- Compute arity of term constructor


-- | Turn a predicate into a filter
match :: (Typeable a, Typeable b) => (a -> Bool) -> b -> Maybe a
match f = Nothing `mkQ` (\ a -> if f a then Just a else Nothing)



-- | Turn a predicate into a ticker
tick :: (Typeable a, Typeable b) => (a -> Bool) -> b -> Int
tick f = 0 `mkQ` (\a -> if f a then 1 else 0)



-- | Turn a ticker into a counter
count :: (Typeable a, Data b) => (a -> Bool) -> b -> Int
count f = everything (+) (tick f)



-- | Lift a monomorphic predicate to the polymorphic level
alike :: (Typeable a, Typeable b) => (a -> Bool) -> b -> Bool
alike f = False `mkQ` f



