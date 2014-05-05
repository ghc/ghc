{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, OverloadedStrings,
    Rank2Types, RecordWildCards #-}
-- |
-- Module      :  Data.Attoparsec.Internal.Types
-- Copyright   :  Bryan O'Sullivan 2007-2011
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators, loosely based on the Parsec
-- library.

module Data.Attoparsec.Internal.Types
    (
      Parser(..)
    , Failure
    , Success
    , IResult(..)
    , Input(..)
    , Added(..)
    , More(..)
    , addS
    , (<>)
    ) where

import Control.Applicative (Alternative(..), Applicative(..), (<$>))
import Control.DeepSeq (NFData(rnf))
import Control.Monad (MonadPlus(..))
import Data.Monoid (Monoid(..))
import Prelude hiding (getChar, take, takeWhile)

-- | The result of a parse.  This is parameterised over the type @t@
-- of string that was processed.
--
-- This type is an instance of 'Functor', where 'fmap' transforms the
-- value in a 'Done' result.
data IResult t r = Fail t [String] String
                 -- ^ The parse failed.  The 't' parameter is the
                 -- input that had not yet been consumed when the
                 -- failure occurred.  The @[@'String'@]@ is a list of
                 -- contexts in which the error occurred.  The
                 -- 'String' is the message describing the error, if
                 -- any.
                 | Partial (t -> IResult t r)
                 -- ^ Supply this continuation with more input so that
                 -- the parser can resume.  To indicate that no more
                 -- input is available, use an empty string.
                 | Done t r
                 -- ^ The parse succeeded.  The 't' parameter is the
                 -- input that had not yet been consumed (if any) when
                 -- the parse succeeded.

instance (Show t, Show r) => Show (IResult t r) where
    show (Fail t stk msg) =
        "Fail " ++ show t ++ " " ++ show stk ++ " " ++ show msg
    show (Partial _)      = "Partial _"
    show (Done t r)       = "Done " ++ show t ++ " " ++ show r

instance (NFData t, NFData r) => NFData (IResult t r) where
    rnf (Fail t stk msg) = rnf t `seq` rnf stk `seq` rnf msg
    rnf (Partial _)  = ()
    rnf (Done t r)   = rnf t `seq` rnf r
    {-# INLINE rnf #-}

fmapR :: (a -> b) -> IResult t a -> IResult t b
fmapR _ (Fail t stk msg) = Fail t stk msg
fmapR f (Partial k)       = Partial (fmapR f . k)
fmapR f (Done t r)       = Done t (f r)

instance Functor (IResult t) where
    fmap = fmapR
    {-# INLINE fmap #-}

newtype Input t = I {unI :: t} deriving (Monoid)
newtype Added t = A {unA :: t} deriving (Monoid)

-- | The core parser type.  This is parameterised over the type @t@ of
-- string being processed.
--
-- This type is an instance of the following classes:
--
-- * 'Monad', where 'fail' throws an exception (i.e. fails) with an
--   error message.
--
-- * 'Functor' and 'Applicative', which follow the usual definitions.
--
-- * 'MonadPlus', where 'mzero' fails (with no error message) and
--   'mplus' executes the right-hand parser if the left-hand one
--   fails.  When the parser on the right executes, the input is reset
--   to the same state as the parser on the left started with. (In
--   other words, Attoparsec is a backtracking parser that supports
--   arbitrary lookahead.)
--
-- * 'Alternative', which follows 'MonadPlus'.
newtype Parser t a = Parser {
      runParser :: forall r. Input t -> Added t -> More
                -> Failure t   r
                -> Success t a r
                -> IResult t r
    }

type Failure t   r = Input t -> Added t -> More -> [String] -> String
                   -> IResult t r
type Success t a r = Input t -> Added t -> More -> a -> IResult t r

-- | Have we read all available input?
data More = Complete | Incomplete
            deriving (Eq, Show)

instance Monoid More where
    mappend c@Complete _ = c
    mappend _ m          = m
    mempty               = Incomplete

addS :: (Monoid t) =>
        Input t -> Added t -> More
     -> Input t -> Added t -> More
     -> (Input t -> Added t -> More -> r) -> r
addS i0 a0 m0 _i1 a1 m1 f =
    let !i = i0 <> I (unA a1)
        a  = a0 <> a1
        !m = m0 <> m1
    in f i a m
{-# INLINE addS #-}

bindP :: Parser t a -> (a -> Parser t b) -> Parser t b
bindP m g =
    Parser $ \i0 a0 m0 kf ks -> runParser m i0 a0 m0 kf $
                                \i1 a1 m1 a -> runParser (g a) i1 a1 m1 kf ks
{-# INLINE bindP #-}

returnP :: a -> Parser t a
returnP a = Parser (\i0 a0 m0 _kf ks -> ks i0 a0 m0 a)
{-# INLINE returnP #-}

instance Monad (Parser t) where
    return = returnP
    (>>=)  = bindP
    fail   = failDesc

noAdds :: (Monoid t) =>
          Input t -> Added t -> More
       -> (Input t -> Added t -> More -> r) -> r
noAdds i0 _a0 m0 f = f i0 mempty m0
{-# INLINE noAdds #-}

plus :: (Monoid t) => Parser t a -> Parser t a -> Parser t a
plus a b = Parser $ \i0 a0 m0 kf ks ->
           let kf' i1 a1 m1 _ _ = addS i0 a0 m0 i1 a1 m1 $
                                  \ i2 a2 m2 -> runParser b i2 a2 m2 kf ks
               ks' i1 a1 m1 = ks i1 (a0 <> a1) m1
           in  noAdds i0 a0 m0 $ \i2 a2 m2 -> runParser a i2 a2 m2 kf' ks'
{-# INLINE plus #-}

instance (Monoid t) => MonadPlus (Parser t) where
    mzero = failDesc "mzero"
    {-# INLINE mzero #-}
    mplus = plus

fmapP :: (a -> b) -> Parser t a -> Parser t b
fmapP p m = Parser $ \i0 a0 m0 f k ->
            runParser m i0 a0 m0 f $ \i1 a1 s1 a -> k i1 a1 s1 (p a)
{-# INLINE fmapP #-}

instance Functor (Parser t) where
    fmap = fmapP
    {-# INLINE fmap #-}

apP :: Parser t (a -> b) -> Parser t a -> Parser t b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

instance Applicative (Parser t) where
    pure   = returnP
    {-# INLINE pure #-}
    (<*>)  = apP
    {-# INLINE (<*>) #-}

#if MIN_VERSION_base(4,2,0)
    -- These definitions are equal to the defaults, but this
    -- way the optimizer doesn't have to work so hard to figure
    -- that out.
    (*>)   = (>>)
    {-# INLINE (*>) #-}
    x <* y = x >>= \a -> y >> return a
    {-# INLINE (<*) #-}
#endif

instance (Monoid t) => Monoid (Parser t a) where
    mempty  = failDesc "mempty"
    {-# INLINE mempty #-}
    mappend = plus
    {-# INLINE mappend #-}

instance (Monoid t) => Alternative (Parser t) where
    empty = failDesc "empty"
    {-# INLINE empty #-}

    (<|>) = plus
    {-# INLINE (<|>) #-}

#if MIN_VERSION_base(4,2,0)
    many v = many_v
        where many_v = some_v <|> pure []
              some_v = (:) <$> v <*> many_v
    {-# INLINE many #-}

    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v
    {-# INLINE some #-}
#endif

failDesc :: String -> Parser t a
failDesc err = Parser (\i0 a0 m0 kf _ks -> kf i0 a0 m0 [] msg)
    where msg = "Failed reading: " ++ err
{-# INLINE failDesc #-}

(<>) :: (Monoid m) => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
