{-# OPTIONS -fglasgow-exts #-}

module Main where
import Data.Generics
import Data.Maybe
import Control.Monad.State


-- Sample datatypes
data T1 = T1a               deriving (Typeable, Data) -- just a constant
data T2 = T2 T1             deriving (Typeable, Data) -- little detour
data T3 = T3a T2 | T3b T3   deriving (Typeable, Data) -- recursive case
data T4 = T4 T3 T3          deriving (Typeable, Data) -- sum matters


-- Purely side-effecting fold over lists
void :: Monad m => (a -> m ()) -> [a] -> m ()
void f = foldM (\() x -> f x) ()


-- Purely side-effecting fold over immediate subterms
gvoid :: (Data a, Monad m) => (forall a. Data a => a -> m ()) -> a -> m ()
gvoid f = unV . gfoldl (\c x -> V (unV c >>= \() -> f x))
                       (const (V (return ())))


-- The void and phantom type constructor
newtype V m a = V { unV :: m () }


-- Type arguments to stipulate use of undefineds
type TypeArg a = a -> ()
typeArg :: TypeArg a
typeArg = const ()


-- Sample type arguments
t1 = typeArg :: TypeArg T1
t2 = typeArg :: TypeArg T2
t3 = typeArg :: TypeArg T3
t4 = typeArg :: TypeArg T4


-- To force two types to be the same;
-- or to construct a type argument
--
testType :: a -> TypeArg a
testType _ = const ()



-- Extend a type function
extTypeFun :: (Data a, Typeable r) => GTypeFun r -> TypeFun a r -> GTypeFun r
extTypeFun f = maybe f id . cast


-- Data structure for collecting information about types
data Type t c = Type
  { perType   :: GTypeFun t
  , perConstr :: GTypeFun (Constr -> c)
  }


{-

The size of types as the smallest size among its constructors;
the size of constructors as the sum of its component sizes;
we use Maybe Int as sizes where Nothing means pessimistic infinity
to cope with the case that the size is not known yet;
we also flag each type with a Bool to block descent (True).

-}

type GSize  = Type (Bool, Maybe Int) (Maybe Int)


-- A completely undefined (say, initial) size structure
isize = Type { perType   = const (False, Nothing)
             , perConstr = const $ const Nothing
             }


-- The transitive-closure function to determine the size of a type
gsize :: GTypeFun (State GSize ())
gsize (ta::a->())
  = do 
       s <- get
       ( if or [ isJust $ snd $ perType s ta  -- size known
               , fst $ perType s ta           -- descent blocked
               ]
           then return ()
           else 
             ( do
                  flagType True
                  stepType
                  flagType False
                  gsize ta ) )

  where

    -- Block a type for further descent
    flagType :: Bool -> State GSize ()
    flagType f
      = do s <- get
           put $ s { perType = perType s
                     `extTypeFun`
                     ( \(_::a->()) -> ( f, snd $ perType s ta ) ) }

    -- the constructors for the type at hand
    cons = dataTypeCons (dataTypeOf (undefinedType ta))


    -- The step function which folds over the constructors
    stepType :: State GSize ()
    stepType
      = do void stepConstr cons
           s <- get
           resizeType $ minConstrs (perConstr s ta) cons


    -- Modify size of a type
    resizeType :: Maybe Int -> State GSize ()
    resizeType size
      = do s <- get
           put $ s { perType = perType s
                           `extTypeFun`
                           ( \(_::a->()) -> ( fst $ perType s ta, size ) ) }


    -- Determine minimum among the sizes of constructors
    minConstrs :: (Constr -> Maybe Int) -> [Constr] -> Maybe Int
    minConstrs f = foldr (minJust . f) Nothing


    -- The step function which processes a given constructor
    stepConstr :: Constr -> State GSize ()
    stepConstr c
      = do gvoid (stepComp . testType) term
           s <- get
           resizeConstr $ sumComps s

      where

        -- Term constructed from c
        term = withType (fromConstr c) ta


        -- Modify size of a constructor
        resizeConstr :: Maybe Int -> State GSize ()
        resizeConstr size
          = do s <- get 
               put $ s { perConstr = perConstr s
                                     `extTypeFun`
                                     ( \(_::a->()) c' -> 
                                       if c==c'
                                         then size
                                         else perConstr s ta c' ) }


        -- Compute constructor size as sum of component sizes + 1
        sumComps :: GSize -> Maybe Int
        sumComps s = foldr addJust (Just 1)
                   $ gmapQ (snd . perType s . testType) term


    -- The step function which processes a given component
    stepComp :: GTypeFun (State GSize ())
    stepComp = gsize


-- Minimum on maybes with Nothing representing infinite
minJust (Just x) (Just y) = Just (min x y)
minJust x Nothing         = x
minJust Nothing  x        = x


-- Cantor's addition
addJust (Just x) (Just y) = Just (x + y)
addJust _ _ = Nothing


-- Query size of some datatypes
main = print $ ( sizeOfType t1
               , sizeOfType t2
               , sizeOfType t3
               , sizeOfType t4
               )
 where 
   sizeOfType ta = fromJust
                 $ snd 
                 $ perType (snd (runState (gsize ta) isize)) ta
