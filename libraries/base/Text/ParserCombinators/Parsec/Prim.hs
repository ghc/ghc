-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Prim
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  daan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- The primitive parser combinators.
-- 
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Prim
                   ( -- operators: label a parser, alternative
                     (<?>), (<|>)

                   -- basic types
                   , Parser, GenParser
                   , runParser, parse, parseFromFile, parseTest
                   
                   -- primitive parsers:
                   -- instance Functor Parser     : fmap
                   -- instance Monad Parser       : return, >>=, fail
                   -- instance MonadPlus Parser   : mzero (pzero), mplus (<|>)
                   , token, tokens, tokenPrim
                   , try, label, labels, unexpected, pzero

                   -- primitive because of space behaviour
                   , many, skipMany
                                
                   -- user state manipulation
                   , getState, setState, updateState

                   -- state manipulation
                   , getPosition, setPosition
                   , getInput, setInput                   
                   , getParserState, setParserState 
                 ) where

import Prelude
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Control.Monad

{-# INLINE parsecMap    #-}
{-# INLINE parsecReturn #-}
{-# INLINE parsecBind   #-}
{-# INLINE parsecZero   #-}
{-# INLINE parsecPlus   #-}
{-# INLINE token        #-}
{-# INLINE tokenPrim    #-}

-----------------------------------------------------------
-- Operators:
-- <?>  gives a name to a parser (which is used in error messages)
-- <|>  is the choice operator
-----------------------------------------------------------
infix  0 <?>
infixr 1 <|>

(<?>) :: GenParser tok st a -> String -> GenParser tok st a
p <?> msg           = label p msg

(<|>) :: GenParser tok st a -> GenParser tok st a -> GenParser tok st a
p1 <|> p2           = mplus p1 p2


-----------------------------------------------------------
-- User state combinators
-----------------------------------------------------------
getState :: GenParser tok st st
getState        = do{ state <- getParserState
                    ; return (stateUser state)
                    }

setState :: st -> GenParser tok st ()
setState st     = do{ updateParserState (\(State input pos _) -> State input pos st)
                    ; return ()
                    }

updateState :: (st -> st) -> GenParser tok st ()
updateState f   = do{ updateParserState (\(State input pos user) -> State input pos (f user))
                    ; return ()
                    }


-----------------------------------------------------------
-- Parser state combinators
-----------------------------------------------------------
getPosition :: GenParser tok st SourcePos
getPosition         = do{ state <- getParserState; return (statePos state) }

getInput :: GenParser tok st [tok]
getInput            = do{ state <- getParserState; return (stateInput state) }


setPosition :: SourcePos -> GenParser tok st ()
setPosition pos     = do{ updateParserState (\(State input _ user) -> State input pos user)
                        ; return ()
                        }
                        
setInput :: [tok] -> GenParser tok st ()
setInput input      = do{ updateParserState (\(State _ pos user) -> State input pos user)
                        ; return ()
                        }

getParserState	    :: GenParser tok st (State tok st)
getParserState      =  updateParserState id    

setParserState	    :: State tok st -> GenParser tok st (State tok st)
setParserState st   = updateParserState (const st)




-----------------------------------------------------------
-- Parser definition.
-- GenParser tok st a:
--  General parser for tokens of type "tok", 
--  a user state "st" and a result type "a"
-----------------------------------------------------------
type Parser a           = GenParser Char () a

newtype GenParser tok st a = Parser (State tok st -> Consumed (Reply tok st a))
runP (Parser p)            = p

data Consumed a         = Consumed a                --input is consumed
                        | Empty !a                  --no input is consumed
                    
data Reply tok st a     = Ok a (State tok st) ParseError      --parsing succeeded with "a"
                        | Error ParseError                    --parsing failed

data State tok st       = State { stateInput :: [tok]
                                , statePos   :: SourcePos
                                , stateUser  :: !st
                                }


-----------------------------------------------------------
-- run a parser
-----------------------------------------------------------
parseFromFile :: Parser a -> SourceName -> IO (Either ParseError a)
parseFromFile p fname
    = do{ input <- readFile fname
        ; return (parse p fname input)
        }

parseTest :: Show a => GenParser tok () a -> [tok] -> IO ()
parseTest p input
    = case (runParser p () "" input) of
        Left err -> do{ putStr "parse error at "
                      ; print err
                      }
        Right x  -> print x


parse :: GenParser tok () a -> SourceName -> [tok] -> Either ParseError a
parse p name input
    = runParser p () name input


runParser :: GenParser tok st a -> st -> SourceName -> [tok] -> Either ParseError a
runParser p st name input
    = case parserReply (runP p (State input (initialPos name) st)) of
        Ok x _ _    -> Right x
        Error err   -> Left err

parserReply result     
    = case result of
        Consumed reply -> reply
        Empty reply    -> reply


-----------------------------------------------------------
-- Functor: fmap
-----------------------------------------------------------
instance Functor (GenParser tok st) where
  fmap f p  = parsecMap f p

parsecMap :: (a -> b) -> GenParser tok st a -> GenParser tok st b
parsecMap f (Parser p)
    = Parser (\state -> 
        case (p state) of
          Consumed reply -> Consumed (mapReply reply)
          Empty    reply -> Empty    (mapReply reply)
      )
    where
      mapReply reply
        = case reply of
            Ok x state err -> let fx = f x 
                              in seq fx (Ok fx state err)
            Error err      -> Error err
           

-----------------------------------------------------------
-- Monad: return, sequence (>>=) and fail
-----------------------------------------------------------    
instance Monad (GenParser tok st) where
  return x   = parsecReturn x  
  p >>= f    = parsecBind p f
  fail msg   = parsecFail msg

parsecReturn :: a -> GenParser tok st a
parsecReturn x
  = Parser (\state -> Empty (Ok x state (unknownError state)))   

parsecBind :: GenParser tok st a -> (a -> GenParser tok st b) -> GenParser tok st b
parsecBind (Parser p) f
    = Parser (\state ->
        case (p state) of                 
          Consumed reply1 
            -> Consumed $
               case (reply1) of
                 Ok x state1 err1 -> case runP (f x) state1 of
                                       Empty reply2    -> mergeErrorReply err1 reply2
                                       Consumed reply2 -> reply2
                 Error err1       -> Error err1

          Empty reply1    
            -> case (reply1) of
                 Ok x state1 err1 -> case runP (f x) state1 of
                                       Empty reply2 -> Empty (mergeErrorReply err1 reply2)
                                       other        -> other                                                    
                 Error err1       -> Empty (Error err1)
      )                                                              

mergeErrorReply err1 reply
  = case reply of
      Ok x state err2 -> Ok x state (mergeError err1 err2)
      Error err2      -> Error (mergeError err1 err2)


parsecFail :: String -> GenParser tok st a
parsecFail msg
  = Parser (\state -> 
      Empty (Error (newErrorMessage (Message msg) (statePos state))))


-----------------------------------------------------------
-- MonadPlus: alternative (mplus) and mzero
-----------------------------------------------------------
instance MonadPlus (GenParser tok st) where
  mzero         = parsecZero
  mplus p1 p2   = parsecPlus p1 p2
      

pzero :: GenParser tok st a
pzero = parsecZero

parsecZero :: GenParser tok st a
parsecZero
    = Parser (\state -> Empty (Error (unknownError state)))

parsecPlus :: GenParser tok st a -> GenParser tok st a -> GenParser tok st a
parsecPlus (Parser p1) (Parser p2)
    = Parser (\state ->
        case (p1 state) of        
          Empty (Error err) -> case (p2 state) of
                                 Empty reply -> Empty (mergeErrorReply err reply)
                                 consumed    -> consumed
          other             -> other
      )


{- 
-- variant that favors a consumed reply over an empty one, even it is not the first alternative.
          empty@(Empty reply) -> case reply of
                                   Error err ->
                                     case (p2 state) of
                                       Empty reply -> Empty (mergeErrorReply err reply)
                                       consumed    -> consumed
                                   ok ->
                                     case (p2 state) of
                                       Empty reply -> empty
                                       consumed    -> consumed
          consumed  -> consumed
-}


-----------------------------------------------------------
-- Primitive Parsers: 
--  try, token(Prim), label, unexpected and updateState
-----------------------------------------------------------
try :: GenParser tok st a -> GenParser tok st a
try (Parser p)
    = Parser (\state@(State input pos user) ->     
        case (p state) of
          Consumed (Error err)  -> Empty (Error (setErrorPos pos err))
          Consumed ok           -> Consumed ok    -- was: Empty ok
          empty                 -> empty
      )

     
token :: (tok -> String) -> (tok -> SourcePos) -> (tok -> Maybe a) -> GenParser tok st a    
token show tokpos test
  = tokenPrim show nextpos test
  where
    nextpos _ _   (tok:toks)  = tokpos tok
    nextpos _ tok []          = tokpos tok

tokenPrim :: (tok -> String) -> (SourcePos -> tok -> [tok] -> SourcePos) -> (tok -> Maybe a) -> GenParser tok st a
tokenPrim show nextpos test
    = Parser (\state@(State input pos user) -> 
        case input of
          (c:cs) -> case test c of
                      Just x  -> let newpos   = nextpos pos c cs
                                     newstate = State cs newpos user
                                 in seq newpos $ seq newstate $ 
                                    Consumed (Ok x newstate (newErrorUnknown newpos))
                      Nothing -> Empty (sysUnExpectError (show c) pos)
          []     -> Empty (sysUnExpectError "" pos)
      )


label :: GenParser tok st a -> String -> GenParser tok st a    
label p msg
  = labels p [msg]

labels :: GenParser tok st a -> [String] -> GenParser tok st a
labels (Parser p) msgs
    = Parser (\state -> 
        case (p state) of
          Empty reply -> Empty $ 
                         case (reply) of
                           Error err        -> Error (setExpectErrors err msgs)
                           Ok x state1 err  | errorIsUnknown err -> reply
                                            | otherwise -> Ok x state1 (setExpectErrors err msgs)
          other       -> other
      )


updateParserState :: (State tok st -> State tok st) -> GenParser tok st (State tok st)
updateParserState f 
    = Parser (\state -> let newstate = f state
                        in seq newstate $
                           Empty (Ok state newstate (unknownError newstate)))
    
    
unexpected :: String -> GenParser tok st a
unexpected msg
    = Parser (\state -> Empty (Error (newErrorMessage (UnExpect msg) (statePos state))))
    

setExpectErrors err []         = setErrorMessage (Expect "") err
setExpectErrors err [msg]      = setErrorMessage (Expect msg) err
setExpectErrors err (msg:msgs) = foldr (\msg err -> addErrorMessage (Expect msg) err) 
                                       (setErrorMessage (Expect msg) err) msgs

sysUnExpectError msg pos  = Error (newErrorMessage (SysUnExpect msg) pos)
unknownError state        = newErrorUnknown (statePos state)

-----------------------------------------------------------
-- Parsers unfolded for space:
-- if many and skipMany are not defined as primitives,
-- they will overflow the stack on large inputs
-----------------------------------------------------------    
many :: GenParser tok st a -> GenParser tok st [a]
many p
  = do{ xs <- manyAccum (:) p
      ; return (reverse xs)
      }

skipMany :: GenParser tok st a -> GenParser tok st ()
skipMany p
  = do{ manyAccum (\x xs -> []) p
      ; return ()
      }

manyAccum :: (a -> [a] -> [a]) -> GenParser tok st a -> GenParser tok st [a]
manyAccum accum (Parser p)
  = Parser (\state -> 
    let walk xs state r = case r of
                           Empty (Error err)          -> Ok xs state err
                           Empty ok                   -> error "Text.ParserCombinators.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."
                           Consumed (Error err)       -> Error err
                           Consumed (Ok x state' err) -> let ys = accum x xs
                                                         in seq ys (walk ys state' (p state'))
    in case (p state) of
         Empty reply  -> case reply of
                           Ok x state' err -> error "Text.ParserCombinators.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."
                           Error err       -> Empty (Ok [] state err)
         consumed     -> Consumed $ walk [] state consumed)



-----------------------------------------------------------
-- Parsers unfolded for speed: 
--  tokens
-----------------------------------------------------------    

{- specification of @tokens@:
tokens showss nextposs s
  = scan s
  where
    scan []       = return s
    scan (c:cs)   = do{ token show nextpos c <?> shows s; scan cs }                      

    show c        = shows [c]
    nextpos pos c = nextposs pos [c]
-}

tokens :: Eq tok => ([tok] -> String) -> (SourcePos -> [tok] -> SourcePos) -> [tok] -> GenParser tok st [tok]
tokens shows nextposs s
    = Parser (\state@(State input pos user) -> 
       let
        ok cs             = let newpos   = nextposs pos s
                                newstate = State cs newpos user
                            in seq newpos $ seq newstate $ 
                               (Ok s newstate (newErrorUnknown newpos))
                               
        errEof            = Error (setErrorMessage (Expect (shows s))
                                     (newErrorMessage (SysUnExpect "") pos))
        errExpect c       = Error (setErrorMessage (Expect (shows s))
                                     (newErrorMessage (SysUnExpect (shows [c])) pos))

        walk [] cs        = ok cs
        walk xs []        = errEof
        walk (x:xs) (c:cs)| x == c        = walk xs cs
                          | otherwise     = errExpect c

        walk1 [] cs        = Empty (ok cs)
        walk1 xs []        = Empty (errEof)
        walk1 (x:xs) (c:cs)| x == c        = Consumed (walk xs cs)
                           | otherwise     = Empty (errExpect c)

       in walk1 s input)


