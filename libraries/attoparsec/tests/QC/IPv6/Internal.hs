-- -----------------------------------------------------------------------------

-- |
-- Module      :  Text.IPv6Addr
-- Copyright   :  Copyright © Michel Boucey 2011-2015
-- License     :  BSD-Style
-- Maintainer  :  michel.boucey@gmail.com
--
-- Dealing with IPv6 address text representations, canonization and manipulations.
--

-- -----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module QC.IPv6.Internal where {-
    ( expandTokens
    , macAddr
    , maybeIPv6AddrTokens
    , ipv4AddrToIPv6AddrTokens
    , ipv6TokensToText
    , ipv6TokensToIPv6Addr
    , isIPv6Addr
    , maybeTokIPv6Addr
    , maybeTokPureIPv6Addr
    , fromDoubleColon
    , fromIPv6Addr
    , toDoubleColon
    ) where -}

import Debug.Trace
import Control.Monad (replicateM)
import Data.Attoparsec.Text
import Data.Char (isDigit,isHexDigit,toLower)
import Data.Monoid ((<>))
import Control.Applicative ((<|>),(<*))
import Data.List (group,isSuffixOf,elemIndex,elemIndices,intersperse)
import Data.Word (Word32)
import Numeric (showHex)
import qualified Data.Text as T
import qualified Data.Text.Read as R (decimal)
import Data.Maybe (fromJust)

import QC.IPv6.Types

tok0 = "0"

-- | Returns the 'T.Text' of an IPv6 address.
fromIPv6Addr :: IPv6Addr -> T.Text
fromIPv6Addr (IPv6Addr t) = t

-- | Given an arbitrary list of 'IPv6AddrToken', returns the corresponding 'T.Text'.
ipv6TokensToText :: [IPv6AddrToken] -> T.Text
ipv6TokensToText l = T.concat $ map ipv6TokenToText l

-- | Returns the corresponding 'T.Text' of an IPv6 address token.
ipv6TokenToText :: IPv6AddrToken -> T.Text
ipv6TokenToText (SixteenBit s) = s
ipv6TokenToText Colon = ":"
ipv6TokenToText DoubleColon = "::"
ipv6TokenToText AllZeros = tok0 -- "A single 16-bit 0000 field MUST be represented as 0" (RFC 5952, 4.1)
ipv6TokenToText (IPv4Addr a) = a

-- | Returns 'True' if a list of 'IPv6AddrToken' constitutes a valid IPv6 Address.
isIPv6Addr :: [IPv6AddrToken] -> Bool
isIPv6Addr [] = False
isIPv6Addr [DoubleColon] = True
isIPv6Addr [DoubleColon,SixteenBit tok1] = True
isIPv6Addr tks =
    diffNext tks && (do
        let cdctks = countDoubleColon tks
        let lentks = length tks
        let lasttk = last tks
        let lenconst = (lentks == 15 && cdctks == 0) || (lentks < 15 && cdctks == 1)
        firstValidToken tks &&
            (case countIPv4Addr tks of
                0 -> case lasttk of
                         SixteenBit _ -> lenconst
                         DoubleColon  -> lenconst
                         AllZeros     -> lenconst
                         _            -> False
                1 -> case lasttk of
                         IPv4Addr _ -> (lentks == 13 && cdctks == 0) || (lentks < 12 && cdctks == 1)
                         _          -> False
                otherwise -> False))
          where diffNext [] = False
                diffNext [_] = True
                diffNext (t:ts) = do
                    let h = head ts
                    case t of
                        SixteenBit _ -> case h of
                                            SixteenBit _ -> False
                                            AllZeros     -> False
                                            _            -> diffNext ts
                        AllZeros     -> case h of
                                            SixteenBit _ -> False
                                            AllZeros     -> False
                                            _            -> diffNext ts
                        _            -> diffNext ts
                firstValidToken l =
                    case head l of
                        SixteenBit _ -> True
                        DoubleColon  -> True
                        AllZeros     -> True
                        _            -> False
                countDoubleColon l = length $ elemIndices DoubleColon l
                tok1 = "1"

countIPv4Addr = foldr oneMoreIPv4Addr 0
  where
    oneMoreIPv4Addr t c = case t of
                              IPv4Addr _ -> c + 1
                              otherwise  -> c

-- | This is the main function which returns 'Just' the list of a tokenized IPv6
-- address text representation validated against RFC 4291 and canonized
-- in conformation with RFC 5952, or 'Nothing'.
maybeTokIPv6Addr :: T.Text -> Maybe [IPv6AddrToken]
maybeTokIPv6Addr t =
    case maybeIPv6AddrTokens t of
        Just ltks -> if isIPv6Addr ltks
                         then Just $ (ipv4AddrReplacement . toDoubleColon . fromDoubleColon) ltks
                         else Nothing
        Nothing   -> Nothing
  where
    ipv4AddrReplacement ltks =
        if ipv4AddrRewrite ltks
            then init ltks ++ ipv4AddrToIPv6AddrTokens (last ltks)
            else ltks

-- | Returns 'Just' the list of tokenized pure IPv6 address, always rewriting an
-- embedded IPv4 address if present.
maybeTokPureIPv6Addr :: T.Text -> Maybe [IPv6AddrToken]
maybeTokPureIPv6Addr t = do
    ltks <- maybeIPv6AddrTokens t
    if isIPv6Addr ltks
        then Just $ (toDoubleColon . ipv4AddrReplacement . fromDoubleColon) ltks
        else Nothing
  where
    ipv4AddrReplacement ltks' = init ltks' ++ ipv4AddrToIPv6AddrTokens (last ltks')

-- | Tokenize a 'T.Text' into 'Just' a list of 'IPv6AddrToken', or 'Nothing'.
maybeIPv6AddrTokens :: T.Text -> Maybe [IPv6AddrToken]
maybeIPv6AddrTokens s =
    case readText s of
         Done r l -> traceShow (r,l) $ if r==T.empty then Just l else Nothing
         Fail {}  -> Nothing
  where
    readText s = feed (parse (many1 $ ipv4Addr <|> sixteenBit <|> doubleColon <|> colon) s) T.empty

-- | An embedded IPv4 address have to be rewritten to output a pure IPv6 Address
-- text representation in hexadecimal digits. But some well-known prefixed IPv6
-- addresses have to keep visible in their text representation the fact that
-- they deals with IPv4 to IPv6 transition process (RFC 5952 Section 5):
--
-- IPv4-compatible IPv6 address like "::1.2.3.4"
--
-- IPv4-mapped IPv6 address like "::ffff:1.2.3.4"
--
-- IPv4-translated address like "::ffff:0:1.2.3.4"
--
-- IPv4-translatable address like "64:ff9b::1.2.3.4"
--
-- ISATAP address like "fe80::5efe:1.2.3.4"
--
ipv4AddrRewrite :: [IPv6AddrToken] -> Bool
ipv4AddrRewrite tks =
    case last tks of
        IPv4Addr _ -> do
            let itks = init tks
            not (itks == [DoubleColon]
                 || itks == [DoubleColon,SixteenBit tokffff,Colon]
                 || itks == [DoubleColon,SixteenBit tokffff,Colon,AllZeros,Colon]
                 || itks == [SixteenBit "64",Colon,SixteenBit "ff9b",DoubleColon]
                 || [SixteenBit "200",Colon,SixteenBit tok5efe,Colon] `isSuffixOf` itks
                 || [AllZeros,Colon,SixteenBit tok5efe,Colon] `isSuffixOf` itks
                 || [DoubleColon,SixteenBit tok5efe,Colon] `isSuffixOf` itks)
        _          -> False
  where
    tokffff = "ffff"
    tok5efe = "5efe"

-- | Rewrites an embedded 'IPv4Addr' into the corresponding list of pure 'IPv6Addr' tokens.
--
-- > ipv4AddrToIPv6AddrTokens (IPv4Addr "127.0.0.1") == [SixteenBits "7f0",Colon,SixteenBits "1"]
--
ipv4AddrToIPv6AddrTokens :: IPv6AddrToken -> [IPv6AddrToken]
ipv4AddrToIPv6AddrTokens t =
    case t of
        IPv4Addr a -> do
            let m = toHex a
            [  SixteenBit ((!!) m 0 <> addZero ((!!) m 1))
             , Colon
             , SixteenBit ((!!) m 2 <> addZero ((!!) m 3)) ]
        _          -> [t]
      where
        toHex a = map (\x -> T.pack $ showHex (read (T.unpack x)::Int) "") $ T.split (=='.') a
        addZero d = if T.length d == 1 then tok0 <> d else d

expandTokens :: [IPv6AddrToken] -> [IPv6AddrToken]
expandTokens = map expandToken
  where expandToken (SixteenBit s) = SixteenBit $ T.justifyRight 4 '0' s
        expandToken AllZeros = SixteenBit "0000"
        expandToken t = t

fromDoubleColon :: [IPv6AddrToken] -> [IPv6AddrToken]
fromDoubleColon tks =
    if DoubleColon `notElem` tks
        then tks
        else do let s = splitAt (fromJust $ elemIndex DoubleColon tks) tks
                let fsts = fst s
                let snds = if not (null (snd s)) then tail(snd s) else []
                let fste = if null fsts then [] else fsts ++ [Colon]
                let snde = if null snds then [] else Colon : snds
                fste ++ allZerosTokensReplacement(quantityOfAllZerosTokenToReplace tks) ++ snde
      where
        allZerosTokensReplacement x = intersperse Colon (replicate x AllZeros)
        quantityOfAllZerosTokenToReplace x =
            ntks tks - foldl (\c x -> if (x /= DoubleColon) && (x /= Colon) then c+1 else c) 0 x
          where
            ntks tks = if countIPv4Addr tks == 1 then 7 else 8

toDoubleColon :: [IPv6AddrToken] -> [IPv6AddrToken]
toDoubleColon tks =
    zerosToDoubleColon tks (zerosRunToReplace $ zerosRunsList tks)
  where
    zerosToDoubleColon :: [IPv6AddrToken] -> (Int,Int) -> [IPv6AddrToken]
    -- No all zeros token, so no double colon replacement...
    zerosToDoubleColon ls (_,0) = ls
    -- "The symbol '::' MUST NOT be used to shorten just one 16-bit 0 field" (RFC 5952 4.2.2)
    zerosToDoubleColon ls (_,1) = ls
    zerosToDoubleColon ls (i,l) =
        let ls' = filter (/= Colon) ls
        in intersperse Colon (Prelude.take i ls') ++ [DoubleColon] ++ intersperse Colon (drop (i+l) ls')
    zerosRunToReplace t =
        let l = longestLengthZerosRun t
        in (firstLongestZerosRunIndex t l,l)
      where
        firstLongestZerosRunIndex x y = sum . snd . unzip $ Prelude.takeWhile (/=(True,y)) x
        longestLengthZerosRun x =
            maximum $ map longest x
          where longest t = case t of
                                (True,i)  -> i
                                _         -> 0
    zerosRunsList x = map helper $ groupZerosRuns x
      where
        helper h = (head h == AllZeros, lh) where lh = length h
        groupZerosRuns = group . filter (/= Colon)

ipv6TokensToIPv6Addr :: [IPv6AddrToken] -> Maybe IPv6Addr
ipv6TokensToIPv6Addr l = Just $ IPv6Addr $ ipv6TokensToText l

fullSixteenBit :: T.Text -> Maybe IPv6AddrToken
fullSixteenBit t =
    case parse ipv6AddrFullChunk t of
        Done a b  -> if a==T.empty then Just $ SixteenBit $ T.pack b else Nothing
        _         -> Nothing

macAddr :: Parser (Maybe [IPv6AddrToken])
macAddr = do
    n1 <- count 2 hexaChar <* ":"
    n2 <- count 2 hexaChar <* ":"
    n3 <- count 2 hexaChar <* ":"
    n4 <- count 2 hexaChar <* ":"
    n5 <- count 2 hexaChar <* ":"
    n6 <- count 2 hexaChar
    return $ maybeIPv6AddrTokens $ T.pack $ concat [n1,n2,n3,n4,n5,n6]

sixteenBit :: Parser IPv6AddrToken
sixteenBit = do
    r <- ipv6AddrFullChunk <|> count 3 hexaChar <|> count 2 hexaChar <|> count 1 hexaChar
    -- "Leading zeros MUST be suppressed" (RFC 5952, 4.1)
    let r' = T.dropWhile (=='0') $ T.pack r
    return $ if T.null r'
                 then AllZeros
                 -- Hexadecimal digits MUST be in lowercase (RFC 5952 4.3)
                 else SixteenBit $ T.toLower r'

ipv4Addr :: Parser IPv6AddrToken
ipv4Addr = do
    n1 <- manyDigits <* "."
    if n1 /= T.empty
        then do n2 <- manyDigits <* "."
                if n2 /= T.empty
                    then do n3 <- manyDigits <* "."
                            if n3 /= T.empty
                                then do n4 <- manyDigits
                                        if n4 /= T.empty
                                            then return $ IPv4Addr $ T.intercalate "." [n1,n2,n3,n4]
                                            else parserFailure
                                else parserFailure
                    else parserFailure
        else parserFailure
  where
    parserFailure = fail "ipv4Addr parsing failure"
    manyDigits = do
      ds <- takeWhile1 isDigit
      case R.decimal ds of
          Right (n,_) -> return (if n < 256 then T.pack $ show n else T.empty)
          Left  _     -> return T.empty

doubleColon :: Parser IPv6AddrToken
doubleColon = do
    string "::"
    return DoubleColon

colon :: Parser IPv6AddrToken
colon = do
    string ":"
    return Colon

ipv6AddrFullChunk :: Parser String
ipv6AddrFullChunk = count 4 hexaChar

hexaChar :: Parser Char
hexaChar = satisfy (inClass "0-9a-fA-F")
