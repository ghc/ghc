{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Prim
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The primitive parser combinators.
--
-----------------------------------------------------------------------------

{-# OPTIONS_HADDOCK not-home #-}

module Text.Parsec.Prim
    ( unknownError
    , sysUnExpectError
    , unexpected
    , ParsecT
    , runParsecT
    , mkPT
    , Parsec
    , Consumed(..)
    , Reply(..)
    , State(..)
    , parsecMap
    , parserReturn
    , parserBind
    , mergeErrorReply
    , parserFail
    , parserZero
    , parserPlus
    , (<?>)
    , (<|>)
    , label
    , labels
    , lookAhead
    , Stream(..)
    , tokens
    , try
    , token
    , tokenPrim
    , tokenPrimEx
    , many
    , skipMany
    , manyAccum
    , runPT
    , runP
    , runParserT
    , runParser
    , parse
    , parseTest
    , getPosition
    , getInput
    , setPosition
    , setInput
    , getParserState
    , setParserState
    , updateParserState
    , getState
    , putState
    , modifyState
    , setState
    , updateState
    ) where


import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C

import Data.Typeable ( Typeable )

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL

import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Control.Monad()
import Control.Monad.Trans
import Control.Monad.Identity
import qualified Control.Monad.Fail as Fail

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Cont.Class
import Control.Monad.Error.Class

import Text.Parsec.Pos
import Text.Parsec.Error

unknownError :: State s u -> ParseError
unknownError state        = newErrorUnknown (statePos state)

sysUnExpectError :: String -> SourcePos -> Reply s u a
sysUnExpectError msg pos  = Error (newErrorMessage (SysUnExpect msg) pos)

-- | The parser @unexpected msg@ always fails with an unexpected error
-- message @msg@ without consuming any input.
--
-- The parsers 'fail', ('<?>') and @unexpected@ are the three parsers
-- used to generate error messages. Of these, only ('<?>') is commonly
-- used. For an example of the use of @unexpected@, see the definition
-- of 'Text.Parsec.Combinator.notFollowedBy'.

unexpected :: (Stream s m t) => String -> ParsecT s u m a
unexpected msg
    = ParsecT $ \s _ _ _ eerr ->
      eerr $ newErrorMessage (UnExpect msg) (statePos s)

-- | ParserT monad transformer and Parser type

-- | @ParsecT s u m a@ is a parser with stream type @s@, user state type @u@,
-- underlying monad @m@ and return type @a@.  Parsec is strict in the user state.
-- If this is undesirable, simply used a data type like @data Box a = Box a@ and
-- the state type @Box YourStateType@ to add a level of indirection.

newtype ParsecT s u m a
    = ParsecT {unParser :: forall b .
                 State s u
              -> (a -> State s u -> ParseError -> m b) -- consumed ok
              -> (ParseError -> m b)                   -- consumed err
              -> (a -> State s u -> ParseError -> m b) -- empty ok
              -> (ParseError -> m b)                   -- empty err
              -> m b
             }
#if MIN_VERSION_base(4,7,0)
     deriving ( Typeable )
     -- GHC 7.6 doesn't like deriving instances of Typeabl1 for types with
     -- non-* type-arguments.
#endif

-- | Low-level unpacking of the ParsecT type. To run your parser, please look to
-- runPT, runP, runParserT, runParser and other such functions.
runParsecT :: Monad m => ParsecT s u m a -> State s u -> m (Consumed (m (Reply s u a)))
runParsecT p s = unParser p s cok cerr eok eerr
    where cok a s' err = return . Consumed . return $ Ok a s' err
          cerr err = return . Consumed . return $ Error err
          eok a s' err = return . Empty . return $ Ok a s' err
          eerr err = return . Empty . return $ Error err

-- | Low-level creation of the ParsecT type. You really shouldn't have to do this.
mkPT :: Monad m => (State s u -> m (Consumed (m (Reply s u a)))) -> ParsecT s u m a
mkPT k = ParsecT $ \s cok cerr eok eerr -> do
           cons <- k s
           case cons of
             Consumed mrep -> do
                       rep <- mrep
                       case rep of
                         Ok x s' err -> cok x s' err
                         Error err -> cerr err
             Empty mrep -> do
                       rep <- mrep
                       case rep of
                         Ok x s' err -> eok x s' err
                         Error err -> eerr err

type Parsec s u = ParsecT s u Identity

data Consumed a  = Consumed a
                 | Empty !a
    deriving ( Typeable )

data Reply s u a = Ok a !(State s u) ParseError
                 | Error ParseError
    deriving ( Typeable )

data State s u = State {
      stateInput :: s,
      statePos   :: !SourcePos,
      stateUser  :: !u
    }
    deriving ( Typeable )

instance Functor Consumed where
    fmap f (Consumed x) = Consumed (f x)
    fmap f (Empty x)    = Empty (f x)

instance Functor (Reply s u) where
    fmap f (Ok x s e) = Ok (f x) s e
    fmap _ (Error e) = Error e -- XXX

instance Functor (ParsecT s u m) where
    fmap f p = parsecMap f p

parsecMap :: (a -> b) -> ParsecT s u m a -> ParsecT s u m b
parsecMap f p
    = ParsecT $ \s cok cerr eok eerr ->
      unParser p s (cok . f) cerr (eok . f) eerr

instance Applicative.Applicative (ParsecT s u m) where
    pure = parserReturn
    (<*>) = ap -- TODO: Can this be optimized?

instance Applicative.Alternative (ParsecT s u m) where
    empty = mzero
    (<|>) = mplus

instance Monad (ParsecT s u m) where
    return = Applicative.pure
    p >>= f = parserBind p f
    fail = Fail.fail

instance Fail.MonadFail (ParsecT s u m) where
    fail = parserFail

instance (MonadIO m) => MonadIO (ParsecT s u m) where
    liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (ParsecT s u m) where
    ask = lift ask
    local f p = mkPT $ \s -> local f (runParsecT p s)

-- I'm presuming the user might want a separate, non-backtracking
-- state aside from the Parsec user state.
instance (MonadState s m) => MonadState s (ParsecT s' u m) where
    get = lift get
    put = lift . put

instance (MonadCont m) => MonadCont (ParsecT s u m) where
    callCC f = mkPT $ \s ->
          callCC $ \c ->
          runParsecT (f (\a -> mkPT $ \s' -> c (pack s' a))) s

     where pack s a= Empty $ return (Ok a s (unknownError s))

instance (MonadError e m) => MonadError e (ParsecT s u m) where
    throwError = lift . throwError
    p `catchError` h = mkPT $ \s ->
        runParsecT p s `catchError` \e ->
            runParsecT (h e) s

parserReturn :: a -> ParsecT s u m a
parserReturn x
    = ParsecT $ \s _ _ eok _ ->
      eok x s (unknownError s)

parserBind :: ParsecT s u m a -> (a -> ParsecT s u m b) -> ParsecT s u m b
{-# INLINE parserBind #-}
parserBind m k
  = ParsecT $ \s cok cerr eok eerr ->
    let
        -- consumed-okay case for m
        mcok x s err =
            let
                 -- if (k x) consumes, those go straigt up
                 pcok = cok
                 pcerr = cerr

                 -- if (k x) doesn't consume input, but is okay,
                 -- we still return in the consumed continuation
                 peok x s err' = cok x s (mergeError err err')

                 -- if (k x) doesn't consume input, but errors,
                 -- we return the error in the 'consumed-error'
                 -- continuation
                 peerr err' = cerr (mergeError err err')
            in  unParser (k x) s pcok pcerr peok peerr

        -- empty-ok case for m
        meok x s err =
            let
                -- in these cases, (k x) can return as empty
                pcok = cok
                peok x s err' = eok x s (mergeError err err')
                pcerr = cerr
                peerr err' = eerr (mergeError err err')
            in  unParser (k x) s pcok pcerr peok peerr
        -- consumed-error case for m
        mcerr = cerr

        -- empty-error case for m
        meerr = eerr

    in unParser m s mcok mcerr meok meerr


mergeErrorReply :: ParseError -> Reply s u a -> Reply s u a
mergeErrorReply err1 reply -- XXX where to put it?
    = case reply of
        Ok x state err2 -> Ok x state (mergeError err1 err2)
        Error err2      -> Error (mergeError err1 err2)

parserFail :: String -> ParsecT s u m a
parserFail msg
    = ParsecT $ \s _ _ _ eerr ->
      eerr $ newErrorMessage (Message msg) (statePos s)

instance MonadPlus (ParsecT s u m) where
    mzero = parserZero
    mplus p1 p2 = parserPlus p1 p2

-- | @parserZero@ always fails without consuming any input. @parserZero@ is defined
-- equal to the 'mzero' member of the 'MonadPlus' class and to the 'Control.Applicative.empty' member
-- of the 'Control.Applicative.Alternative' class.

parserZero :: ParsecT s u m a
parserZero
    = ParsecT $ \s _ _ _ eerr ->
      eerr $ unknownError s

parserPlus :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
{-# INLINE parserPlus #-}
parserPlus m n
    = ParsecT $ \s cok cerr eok eerr ->
      let
          meerr err =
              let
                  neok y s' err' = eok y s' (mergeError err err')
                  neerr err' = eerr $ mergeError err err'
              in unParser n s cok cerr neok neerr
      in unParser m s cok cerr eok meerr

instance MonadTrans (ParsecT s u) where
    lift amb = ParsecT $ \s _ _ eok _ -> do
               a <- amb
               eok a s $ unknownError s

infix  0 <?>
infixr 1 <|>

-- | The parser @p \<?> msg@ behaves as parser @p@, but whenever the
-- parser @p@ fails /without consuming any input/, it replaces expect
-- error messages with the expect error message @msg@.
--
-- This is normally used at the end of a set alternatives where we want
-- to return an error message in terms of a higher level construct
-- rather than returning all possible characters. For example, if the
-- @expr@ parser from the 'try' example would fail, the error
-- message is: '...: expecting expression'. Without the @(\<?>)@
-- combinator, the message would be like '...: expecting \"let\" or
-- letter', which is less friendly.

(<?>) :: (ParsecT s u m a) -> String -> (ParsecT s u m a)
p <?> msg = label p msg

-- | This combinator implements choice. The parser @p \<|> q@ first
-- applies @p@. If it succeeds, the value of @p@ is returned. If @p@
-- fails /without consuming any input/, parser @q@ is tried. This
-- combinator is defined equal to the 'mplus' member of the 'MonadPlus'
-- class and the ('Control.Applicative.<|>') member of 'Control.Applicative.Alternative'.
--
-- The parser is called /predictive/ since @q@ is only tried when
-- parser @p@ didn't consume any input (i.e.. the look ahead is 1).
-- This non-backtracking behaviour allows for both an efficient
-- implementation of the parser combinators and the generation of good
-- error messages.

(<|>) :: (ParsecT s u m a) -> (ParsecT s u m a) -> (ParsecT s u m a)
p1 <|> p2 = mplus p1 p2

-- | A synonym for @\<?>@, but as a function instead of an operator.
label :: ParsecT s u m a -> String -> ParsecT s u m a
label p msg
  = labels p [msg]

labels :: ParsecT s u m a -> [String] -> ParsecT s u m a
labels p msgs =
    ParsecT $ \s cok cerr eok eerr ->
    let eok' x s' error = eok x s' $ if errorIsUnknown error
                  then error
                  else setExpectErrors error msgs
        eerr' err = eerr $ setExpectErrors err msgs

    in unParser p s cok cerr eok' eerr'

 where
   setExpectErrors err []         = setErrorMessage (Expect "") err
   setExpectErrors err [msg]      = setErrorMessage (Expect msg) err
   setExpectErrors err (msg:msgs)
       = foldr (\msg' err' -> addErrorMessage (Expect msg') err')
         (setErrorMessage (Expect msg) err) msgs

-- TODO: There should be a stronger statement that can be made about this

-- | An instance of @Stream@ has stream type @s@, underlying monad @m@ and token type @t@ determined by the stream
--
-- Some rough guidelines for a \"correct\" instance of Stream:
--
--    * unfoldM uncons gives the [t] corresponding to the stream
--
--    * A @Stream@ instance is responsible for maintaining the \"position within the stream\" in the stream state @s@.  This is trivial unless you are using the monad in a non-trivial way.

class (Monad m) => Stream s m t | s -> t where
    uncons :: s -> m (Maybe (t,s))

instance (Monad m) => Stream [tok] m tok where
    uncons []     = return $ Nothing
    uncons (t:ts) = return $ Just (t,ts)
    {-# INLINE uncons #-}


instance (Monad m) => Stream CL.ByteString m Char where
    uncons = return . CL.uncons

instance (Monad m) => Stream C.ByteString m Char where
    uncons = return . C.uncons

instance (Monad m) => Stream Text.Text m Char where
    uncons = return . Text.uncons
    {-# INLINE uncons #-}

instance (Monad m) => Stream TextL.Text m Char where
    uncons = return . TextL.uncons
    {-# INLINE uncons #-}


tokens :: (Stream s m t, Eq t)
       => ([t] -> String)      -- Pretty print a list of tokens
       -> (SourcePos -> [t] -> SourcePos)
       -> [t]                  -- List of tokens to parse
       -> ParsecT s u m [t]
{-# INLINE tokens #-}
tokens _ _ []
    = ParsecT $ \s _ _ eok _ ->
      eok [] s $ unknownError s
tokens showTokens nextposs tts@(tok:toks)
    = ParsecT $ \(State input pos u) cok cerr _eok eerr ->
    let
        errEof = (setErrorMessage (Expect (showTokens tts))
                  (newErrorMessage (SysUnExpect "") pos))

        errExpect x = (setErrorMessage (Expect (showTokens tts))
                       (newErrorMessage (SysUnExpect (showTokens [x])) pos))

        walk []     rs = ok rs
        walk (t:ts) rs = do
          sr <- uncons rs
          case sr of
            Nothing                 -> cerr $ errEof
            Just (x,xs) | t == x    -> walk ts xs
                        | otherwise -> cerr $ errExpect x

        ok rs = let pos' = nextposs pos tts
                    s' = State rs pos' u
                in cok tts s' (newErrorUnknown pos')
    in do
        sr <- uncons input
        case sr of
            Nothing         -> eerr $ errEof
            Just (x,xs)
                | tok == x  -> walk toks xs
                | otherwise -> eerr $ errExpect x

-- | The parser @try p@ behaves like parser @p@, except that it
-- pretends that it hasn't consumed any input when an error occurs.
--
-- This combinator is used whenever arbitrary look ahead is needed.
-- Since it pretends that it hasn't consumed any input when @p@ fails,
-- the ('<|>') combinator will try its second alternative even when the
-- first parser failed while consuming input.
--
-- The @try@ combinator can for example be used to distinguish
-- identifiers and reserved words. Both reserved words and identifiers
-- are a sequence of letters. Whenever we expect a certain reserved
-- word where we can also expect an identifier we have to use the @try@
-- combinator. Suppose we write:
--
-- >  expr        = letExpr <|> identifier <?> "expression"
-- >
-- >  letExpr     = do{ string "let"; ... }
-- >  identifier  = many1 letter
--
-- If the user writes \"lexical\", the parser fails with: @unexpected
-- \'x\', expecting \'t\' in \"let\"@. Indeed, since the ('<|>') combinator
-- only tries alternatives when the first alternative hasn't consumed
-- input, the @identifier@ parser is never tried (because the prefix
-- \"le\" of the @string \"let\"@ parser is already consumed). The
-- right behaviour can be obtained by adding the @try@ combinator:
--
-- >  expr        = letExpr <|> identifier <?> "expression"
-- >
-- >  letExpr     = do{ try (string "let"); ... }
-- >  identifier  = many1 letter

try :: ParsecT s u m a -> ParsecT s u m a
try p =
    ParsecT $ \s cok _ eok eerr ->
    unParser p s cok eerr eok eerr

-- | @lookAhead p@ parses @p@ without consuming any input.
--
-- If @p@ fails and consumes some input, so does @lookAhead@. Combine with 'try'
-- if this is undesirable.

lookAhead :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a
lookAhead p =
    ParsecT $ \s _ cerr eok eerr -> do
        let eok' a _ _ = eok a s (newErrorUnknown (statePos s))
        unParser p s eok' cerr eok' eerr

-- | The parser @token showTok posFromTok testTok@ accepts a token @t@
-- with result @x@ when the function @testTok t@ returns @'Just' x@. The
-- source position of the @t@ should be returned by @posFromTok t@ and
-- the token can be shown using @showTok t@.
--
-- This combinator is expressed in terms of 'tokenPrim'.
-- It is used to accept user defined token streams. For example,
-- suppose that we have a stream of basic tokens tupled with source
-- positions. We can than define a parser that accepts single tokens as:
--
-- >  mytoken x
-- >    = token showTok posFromTok testTok
-- >    where
-- >      showTok (pos,t)     = show t
-- >      posFromTok (pos,t)  = pos
-- >      testTok (pos,t)     = if x == t then Just t else Nothing

token :: (Stream s Identity t)
      => (t -> String)            -- ^ Token pretty-printing function.
      -> (t -> SourcePos)         -- ^ Computes the position of a token.
      -> (t -> Maybe a)           -- ^ Matching function for the token to parse.
      -> Parsec s u a
token showToken tokpos test = tokenPrim showToken nextpos test
    where
        nextpos _ tok ts = case runIdentity (uncons ts) of
                             Nothing -> tokpos tok
                             Just (tok',_) -> tokpos tok'

-- | The parser @tokenPrim showTok nextPos testTok@ accepts a token @t@
-- with result @x@ when the function @testTok t@ returns @'Just' x@. The
-- token can be shown using @showTok t@. The position of the /next/
-- token should be returned when @nextPos@ is called with the current
-- source position @pos@, the current token @t@ and the rest of the
-- tokens @toks@, @nextPos pos t toks@.
--
-- This is the most primitive combinator for accepting tokens. For
-- example, the 'Text.Parsec.Char.char' parser could be implemented as:
--
-- >  char c
-- >    = tokenPrim showChar nextPos testChar
-- >    where
-- >      showChar x        = "'" ++ x ++ "'"
-- >      testChar x        = if x == c then Just x else Nothing
-- >      nextPos pos x xs  = updatePosChar pos x

tokenPrim :: (Stream s m t)
          => (t -> String)                      -- ^ Token pretty-printing function.
          -> (SourcePos -> t -> s -> SourcePos) -- ^ Next position calculating function.
          -> (t -> Maybe a)                     -- ^ Matching function for the token to parse.
          -> ParsecT s u m a
{-# INLINE tokenPrim #-}
tokenPrim showToken nextpos test = tokenPrimEx showToken nextpos Nothing test

tokenPrimEx :: (Stream s m t)
            => (t -> String)
            -> (SourcePos -> t -> s -> SourcePos)
            -> Maybe (SourcePos -> t -> s -> u -> u)
            -> (t -> Maybe a)
            -> ParsecT s u m a
{-# INLINE tokenPrimEx #-}
tokenPrimEx showToken nextpos Nothing test
  = ParsecT $ \(State input pos user) cok _cerr _eok eerr -> do
      r <- uncons input
      case r of
        Nothing -> eerr $ unexpectError "" pos
        Just (c,cs)
         -> case test c of
              Just x -> let newpos = nextpos pos c cs
                            newstate = State cs newpos user
                        in seq newpos $ seq newstate $
                           cok x newstate (newErrorUnknown newpos)
              Nothing -> eerr $ unexpectError (showToken c) pos
tokenPrimEx showToken nextpos (Just nextState) test
  = ParsecT $ \(State input pos user) cok _cerr _eok eerr -> do
      r <- uncons input
      case r of
        Nothing -> eerr $ unexpectError "" pos
        Just (c,cs)
         -> case test c of
              Just x -> let newpos = nextpos pos c cs
                            newUser = nextState pos c cs user
                            newstate = State cs newpos newUser
                        in seq newpos $ seq newstate $
                           cok x newstate $ newErrorUnknown newpos
              Nothing -> eerr $ unexpectError (showToken c) pos

unexpectError :: String -> SourcePos -> ParseError
unexpectError msg pos = newErrorMessage (SysUnExpect msg) pos


-- | @many p@ applies the parser @p@ /zero/ or more times. Returns a
--    list of the returned values of @p@.
--
-- >  identifier  = do{ c  <- letter
-- >                  ; cs <- many (alphaNum <|> char '_')
-- >                  ; return (c:cs)
-- >                  }

many :: ParsecT s u m a -> ParsecT s u m [a]
many p
  = do xs <- manyAccum (:) p
       return (reverse xs)

-- | @skipMany p@ applies the parser @p@ /zero/ or more times, skipping
-- its result.
--
-- >  spaces  = skipMany space

skipMany :: ParsecT s u m a -> ParsecT s u m ()
skipMany p
  = do _ <- manyAccum (\_ _ -> []) p
       return ()

manyAccum :: (a -> [a] -> [a])
          -> ParsecT s u m a
          -> ParsecT s u m [a]
manyAccum acc p =
    ParsecT $ \s cok cerr eok _eerr ->
    let walk xs x s' _err =
            unParser p s'
              (seq xs $ walk $ acc x xs)  -- consumed-ok
              cerr                        -- consumed-err
              manyErr                     -- empty-ok
              (\e -> cok (acc x xs) s' e) -- empty-err
    in unParser p s (walk []) cerr manyErr (\e -> eok [] s e)

manyErr :: a
manyErr = error "Text.ParserCombinators.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."


-- < Running a parser: monadic (runPT) and pure (runP)

runPT :: (Stream s m t)
      => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runPT p u name s
    = do res <- runParsecT p (State s (initialPos name) u)
         r <- parserReply res
         case r of
           Ok x _ _  -> return (Right x)
           Error err -> return (Left err)
    where
        parserReply res
            = case res of
                Consumed r -> r
                Empty    r -> r

runP :: (Stream s Identity t)
     => Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runP p u name s = runIdentity $ runPT p u name s

-- | The most general way to run a parser. @runParserT p state filePath
-- input@ runs parser @p@ on the input list of tokens @input@,
-- obtained from source @filePath@ with the initial user state @st@.
-- The @filePath@ is only used in error messages and may be the empty
-- string. Returns a computation in the underlying monad @m@ that return either a 'ParseError' ('Left') or a
-- value of type @a@ ('Right').

runParserT :: (Stream s m t)
           => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runParserT = runPT

-- | The most general way to run a parser over the Identity monad. @runParser p state filePath
-- input@ runs parser @p@ on the input list of tokens @input@,
-- obtained from source @filePath@ with the initial user state @st@.
-- The @filePath@ is only used in error messages and may be the empty
-- string. Returns either a 'ParseError' ('Left') or a
-- value of type @a@ ('Right').
--
-- >  parseFromFile p fname
-- >    = do{ input <- readFile fname
-- >        ; return (runParser p () fname input)
-- >        }

runParser :: (Stream s Identity t)
          => Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runParser = runP

-- | @parse p filePath input@ runs a parser @p@ over Identity without user
-- state. The @filePath@ is only used in error messages and may be the
-- empty string. Returns either a 'ParseError' ('Left')
-- or a value of type @a@ ('Right').
--
-- >  main    = case (parse numbers "" "11, 2, 43") of
-- >             Left err  -> print err
-- >             Right xs  -> print (sum xs)
-- >
-- >  numbers = commaSep integer

parse :: (Stream s Identity t)
      => Parsec s () a -> SourceName -> s -> Either ParseError a
parse p = runP p ()

-- | The expression @parseTest p input@ applies a parser @p@ against
-- input @input@ and prints the result to stdout. Used for testing
-- parsers.

parseTest :: (Stream s Identity t, Show a)
          => Parsec s () a -> s -> IO ()
parseTest p input
    = case parse p "" input of
        Left err -> do putStr "parse error at "
                       print err
        Right x  -> print x

-- < Parser state combinators

-- | Returns the current source position. See also 'SourcePos'.

getPosition :: (Monad m) => ParsecT s u m SourcePos
getPosition = do state <- getParserState
                 return (statePos state)

-- | Returns the current input

getInput :: (Monad m) => ParsecT s u m s
getInput = do state <- getParserState
              return (stateInput state)

-- | @setPosition pos@ sets the current source position to @pos@.

setPosition :: (Monad m) => SourcePos -> ParsecT s u m ()
setPosition pos
    = do _ <- updateParserState (\(State input _ user) -> State input pos user)
         return ()

-- | @setInput input@ continues parsing with @input@. The 'getInput' and
-- @setInput@ functions can for example be used to deal with #include
-- files.

setInput :: (Monad m) => s -> ParsecT s u m ()
setInput input
    = do _ <- updateParserState (\(State _ pos user) -> State input pos user)
         return ()

-- | Returns the full parser state as a 'State' record.

getParserState :: (Monad m) => ParsecT s u m (State s u)
getParserState = updateParserState id

-- | @setParserState st@ set the full parser state to @st@.

setParserState :: (Monad m) => State s u -> ParsecT s u m (State s u)
setParserState st = updateParserState (const st)

-- | @updateParserState f@ applies function @f@ to the parser state.

updateParserState :: (State s u -> State s u) -> ParsecT s u m (State s u)
updateParserState f =
    ParsecT $ \s _ _ eok _ ->
    let s' = f s
    in eok s' s' $ unknownError s'

-- < User state combinators

-- | Returns the current user state.

getState :: (Monad m) => ParsecT s u m u
getState = stateUser `liftM` getParserState

-- | @putState st@ set the user state to @st@.

putState :: (Monad m) => u -> ParsecT s u m ()
putState u = do _ <- updateParserState $ \s -> s { stateUser = u }
                return ()

-- | @modifyState f@ applies function @f@ to the user state. Suppose
-- that we want to count identifiers in a source, we could use the user
-- state as:
--
-- >  expr  = do{ x <- identifier
-- >            ; modifyState (+1)
-- >            ; return (Id x)
-- >            }

modifyState :: (Monad m) => (u -> u) -> ParsecT s u m ()
modifyState f = do _ <- updateParserState $ \s -> s { stateUser = f (stateUser s) }
                   return ()

-- XXX Compat

-- | An alias for putState for backwards compatibility.

setState :: (Monad m) => u -> ParsecT s u m ()
setState = putState

-- | An alias for modifyState for backwards compatibility.

updateState :: (Monad m) => (u -> u) -> ParsecT s u m ()
updateState = modifyState
