{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wwarn #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Interface.ParseModuleHeader
-- Copyright   :  (c) Simon Marlow 2006, Isaac Dupree 2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Interface.ParseModuleHeader (parseModuleHeader) where

import Control.Applicative (Alternative (..))
import Control.Monad (ap)
import Data.Char
import GHC.Driver.Session
import Haddock.Parser
import Haddock.Types

-- -----------------------------------------------------------------------------
-- Parsing module headers

-- NB.  The headers must be given in the order Module, Description,
-- Copyright, License, Maintainer, Stability, Portability, except that
-- any or all may be omitted.
parseModuleHeader :: DynFlags -> Maybe Package -> String -> (HaddockModInfo NsRdrName, MDoc NsRdrName)
parseModuleHeader dflags pkgName str0 =
  let
    kvs :: [(String, String)]
    str1 :: String

    (kvs, str1) = maybe ([], str0) id $ runP fields str0

    -- trim whitespaces
    trim :: String -> String
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

    getKey :: String -> Maybe String
    getKey key = fmap trim (lookup key kvs)

    descriptionOpt = getKey "Description"
    copyrightOpt = getKey "Copyright"
    licenseOpt = getKey "License"
    licenceOpt = getKey "Licence"
    spdxLicenceOpt = getKey "SPDX-License-Identifier"
    maintainerOpt = getKey "Maintainer"
    stabilityOpt = getKey "Stability"
    portabilityOpt = getKey "Portability"
   in
    ( HaddockModInfo
        { hmi_description = parseString dflags <$> descriptionOpt
        , hmi_copyright = copyrightOpt
        , hmi_license = spdxLicenceOpt <|> licenseOpt <|> licenceOpt
        , hmi_maintainer = maintainerOpt
        , hmi_stability = stabilityOpt
        , hmi_portability = portabilityOpt
        , hmi_safety = Nothing
        , hmi_language = Nothing -- set in LexParseRn
        , hmi_extensions = [] -- also set in LexParseRn
        }
    , parseParas dflags pkgName str1
    )

-------------------------------------------------------------------------------
-- Small parser to parse module header.
-------------------------------------------------------------------------------

-- | The below is a small parser framework how we read keys.
--
-- all fields in the header are optional and have the form
--
-- [spaces1][field name][spaces] ":"
--    [text]"\n" ([spaces2][space][text]"\n" | [spaces]"\n")*
-- where each [spaces2] should have [spaces1] as a prefix.
--
-- Thus for the key "Description",
--
-- > Description : this is a
-- >    rather long
-- >
-- >    description
-- >
-- > The module comment starts here
--
-- the value will be "this is a .. description" and the rest will begin
-- at "The module comment".

-- | 'C' is a 'Char' carrying its column.
--
-- This let us make an indentation-aware parser, as we know current indentation.
-- by looking at the next character in the stream ('curInd').
--
-- Thus we can munch all spaces but only not-spaces which are indented.
data C = C {-# UNPACK #-} !Int Char

newtype P a = P {unP :: [C] -> Maybe ([C], a)}
  deriving (Functor)

instance Applicative P where
  pure x = P $ \s -> Just (s, x)
  (<*>) = ap

instance Monad P where
  return = pure
  m >>= k = P $ \s0 -> do
    (s1, x) <- unP m s0
    unP (k x) s1

instance Alternative P where
  empty = P $ \_ -> Nothing
  a <|> b = P $ \s -> unP a s <|> unP b s

runP :: P a -> String -> Maybe a
runP p input = fmap snd (unP p input')
  where
    input' =
      concat
        [ zipWith C [0 ..] l ++ [C (length l) '\n']
        | l <- lines input
        ]

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

curInd :: P Int
curInd = P $ \s -> Just . (,) s $ case s of
  [] -> 0
  C i _ : _ -> i

rest :: P String
rest = P $ \cs -> Just ([], [c | C _ c <- cs])

munch :: (Int -> Char -> Bool) -> P String
munch p = P $ \cs ->
  let (xs, ys) = takeWhileMaybe p' cs in Just (ys, xs)
  where
    p' (C i c)
      | p i c = Just c
      | otherwise = Nothing

munch1 :: (Int -> Char -> Bool) -> P String
munch1 p = P $ \s -> case s of
  [] -> Nothing
  (c : cs)
    | Just c' <- p' c -> let (xs, ys) = takeWhileMaybe p' cs in Just (ys, c' : xs)
    | otherwise -> Nothing
  where
    p' (C i c)
      | p i c = Just c
      | otherwise = Nothing

char :: Char -> P Char
char c = P $ \s -> case s of
  [] -> Nothing
  (C _ c' : cs)
    | c == c' -> Just (cs, c)
    | otherwise -> Nothing

skipSpaces :: P ()
skipSpaces = P $ \cs -> Just (dropWhile (\(C _ c) -> isSpace c) cs, ())

takeWhileMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
takeWhileMaybe f = go
  where
    go xs0@[] = ([], xs0)
    go xs0@(x : xs) = case f x of
      Just y -> let (ys, zs) = go xs in (y : ys, zs)
      Nothing -> ([], xs0)

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

field :: Int -> P (String, String)
field i = do
  fn <- munch1 $ \_ c -> isAlpha c || c == '-'
  skipSpaces
  _ <- char ':'
  skipSpaces
  val <- munch $ \j c -> isSpace c || j > i
  return (fn, val)

fields :: P ([(String, String)], String)
fields = do
  skipSpaces
  i <- curInd
  fs <- many (field i)
  r <- rest
  return (fs, r)
