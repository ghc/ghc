{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}
-- |
-- Module      :  Data.Attoparsec.Internal
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators, loosely based on the Parsec
-- library.

module Data.Attoparsec.Internal
    ( compareResults
    , prompt
    , demandInput
    , demandInput_
    , wantInput
    , endOfInput
    , atEnd
    , satisfyElem
    , concatReverse
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (Monoid, mconcat)
#endif
import Data.Attoparsec.Internal.Types
import Data.ByteString (ByteString)
import Data.Text (Text)
import Prelude hiding (succ)

-- | Compare two 'IResult' values for equality.
--
-- If both 'IResult's are 'Partial', the result will be 'Nothing', as
-- they are incomplete and hence their equality cannot be known.
-- (This is why there is no 'Eq' instance for 'IResult'.)
compareResults :: (Eq i, Eq r) => IResult i r -> IResult i r -> Maybe Bool
compareResults (Fail t0 ctxs0 msg0) (Fail t1 ctxs1 msg1) =
    Just (t0 == t1 && ctxs0 == ctxs1 && msg0 == msg1)
compareResults (Done t0 r0) (Done t1 r1) =
    Just (t0 == t1 && r0 == r1)
compareResults (Partial _) (Partial _) = Nothing
compareResults _ _ = Just False

-- | Ask for input.  If we receive any, pass the augmented input to a
-- success continuation, otherwise to a failure continuation.
prompt :: Chunk t
       => State t -> Pos -> More
       -> (State t -> Pos -> More -> IResult t r)
       -> (State t -> Pos -> More -> IResult t r)
       -> IResult t r
prompt t pos _more lose succ = Partial $ \s ->
  if nullChunk s
  then lose t pos Complete
  else succ (pappendChunk t s) pos Incomplete
{-# SPECIALIZE prompt :: State ByteString -> Pos -> More
                      -> (State ByteString -> Pos -> More
                          -> IResult ByteString r)
                      -> (State ByteString -> Pos -> More
                          -> IResult ByteString r)
                      -> IResult ByteString r #-}
{-# SPECIALIZE prompt :: State Text -> Pos -> More
                      -> (State Text -> Pos -> More -> IResult Text r)
                      -> (State Text -> Pos -> More -> IResult Text r)
                      -> IResult Text r #-}

-- | Immediately demand more input via a 'Partial' continuation
-- result.
demandInput :: Chunk t => Parser t ()
demandInput = Parser $ \t pos more lose succ ->
  case more of
    Complete -> lose t pos more [] "not enough input"
    _ -> let lose' _ pos' more' = lose t pos' more' [] "not enough input"
             succ' t' pos' more' = succ t' pos' more' ()
         in prompt t pos more lose' succ'
{-# SPECIALIZE demandInput :: Parser ByteString () #-}
{-# SPECIALIZE demandInput :: Parser Text () #-}

-- | Immediately demand more input via a 'Partial' continuation
-- result.  Return the new input.
demandInput_ :: Chunk t => Parser t t
demandInput_ = Parser $ \t pos more lose succ ->
  case more of
    Complete -> lose t pos more [] "not enough input"
    _ -> Partial $ \s ->
         if nullChunk s
         then lose t pos Complete [] "not enough input"
         else succ (pappendChunk t s) pos more s
{-# SPECIALIZE demandInput_ :: Parser ByteString ByteString #-}
{-# SPECIALIZE demandInput_ :: Parser Text Text #-}

-- | This parser always succeeds.  It returns 'True' if any input is
-- available either immediately or on demand, and 'False' if the end
-- of all input has been reached.
wantInput :: forall t . Chunk t => Parser t Bool
wantInput = Parser $ \t pos more _lose succ ->
  case () of
    _ | pos < atBufferEnd (undefined :: t) t -> succ t pos more True
      | more == Complete -> succ t pos more False
      | otherwise       -> let lose' t' pos' more' = succ t' pos' more' False
                               succ' t' pos' more' = succ t' pos' more' True
                           in prompt t pos more lose' succ'
{-# INLINE wantInput #-}

-- | Match only if all input has been consumed.
endOfInput :: forall t . Chunk t => Parser t ()
endOfInput = Parser $ \t pos more lose succ ->
  case () of
    _| pos < atBufferEnd (undefined :: t) t -> lose t pos more [] "endOfInput"
     | more == Complete -> succ t pos more ()
     | otherwise ->
       let lose' t' pos' more' _ctx _msg = succ t' pos' more' ()
           succ' t' pos' more' _a = lose t' pos' more' [] "endOfInput"
       in  runParser demandInput t pos more lose' succ'
{-# SPECIALIZE endOfInput :: Parser ByteString () #-}
{-# SPECIALIZE endOfInput :: Parser Text () #-}

-- | Return an indication of whether the end of input has been
-- reached.
atEnd :: Chunk t => Parser t Bool
atEnd = not <$> wantInput
{-# INLINE atEnd #-}

satisfySuspended :: forall t r . Chunk t
                 => (ChunkElem t -> Bool)
                 -> State t -> Pos -> More
                 -> Failure t (State t) r
                 -> Success t (State t) (ChunkElem t) r
                 -> IResult t r
satisfySuspended p t pos more lose succ =
    runParser (demandInput >> go) t pos more lose succ
  where go = Parser $ \t' pos' more' lose' succ' ->
          case bufferElemAt (undefined :: t) pos' t' of
            Just (e, l) | p e -> succ' t' (pos' + Pos l) more' e
                        | otherwise -> lose' t' pos' more' [] "satisfyElem"
            Nothing -> runParser (demandInput >> go) t' pos' more' lose' succ'
{-# SPECIALIZE satisfySuspended :: (ChunkElem ByteString -> Bool)
                                -> State ByteString -> Pos -> More
                                -> Failure ByteString (State ByteString) r
                                -> Success ByteString (State ByteString)
                                           (ChunkElem ByteString) r
                                -> IResult ByteString r #-}
{-# SPECIALIZE satisfySuspended :: (ChunkElem Text -> Bool)
                                -> State Text -> Pos -> More
                                -> Failure Text (State Text) r
                                -> Success Text (State Text)
                                           (ChunkElem Text) r
                                -> IResult Text r #-}

-- | The parser @satisfyElem p@ succeeds for any chunk element for which the
-- predicate @p@ returns 'True'. Returns the element that is
-- actually parsed.
satisfyElem :: forall t . Chunk t
            => (ChunkElem t -> Bool) -> Parser t (ChunkElem t)
satisfyElem p = Parser $ \t pos more lose succ ->
    case bufferElemAt (undefined :: t) pos t of
      Just (e, l) | p e -> succ t (pos + Pos l) more e
                  | otherwise -> lose t pos more [] "satisfyElem"
      Nothing -> satisfySuspended p t pos more lose succ
{-# INLINE satisfyElem #-}

-- | Concatenate a monoid after reversing its elements.  Used to
-- glue together a series of textual chunks that have been accumulated
-- \"backwards\".
concatReverse :: Monoid m => [m] -> m
concatReverse [x] = x
concatReverse xs  = mconcat (reverse xs)
{-# INLINE concatReverse #-}
