{-# LANGUAGE CPP #-}

module Distribution.Client.Security.DNS
    ( queryBootstrapMirrors
    ) where

import Prelude ()
import Distribution.Client.Compat.Prelude
import Network.URI (URI(..), URIAuth(..), parseURI)
import Distribution.Verbosity
import Control.Monad
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Distribution.Simple.Utils
import Distribution.Compat.Exception (displayException)

#if defined(MIN_VERSION_resolv) || defined(MIN_VERSION_windns)
import Network.DNS (queryTXT, Name(..), CharStr(..))
import qualified Data.ByteString.Char8 as BS.Char8
#else
import Distribution.Simple.Program.Db
         ( emptyProgramDb, addKnownProgram
         , configureAllKnownPrograms, lookupProgram )
import Distribution.Simple.Program
         ( simpleProgram
         , programInvocation
         , getProgramInvocationOutput )
#endif

-- | Try to lookup RFC1464-encoded mirror urls for a Hackage
-- repository url by performing a DNS TXT lookup on the
-- @_mirrors.@-prefixed URL hostname.
--
-- Example: for @http://hackage.haskell.org/@
-- perform a DNS TXT query for the hostname
-- @_mirrors.hackage.haskell.org@ which may look like e.g.
--
-- > _mirrors.hackage.haskell.org. 300 IN TXT
-- >    "0.urlbase=http://hackage.fpcomplete.com/"
-- >    "1.urlbase=http://objects-us-west-1.dream.io/hackage-mirror/"
--
-- NB: hackage-security doesn't require DNS lookups being trustworthy,
-- as the trust is established via the cryptographically signed TUF
-- meta-data that is retrieved from the resolved Hackage repository.
-- Moreover, we already have to protect against a compromised
-- @hackage.haskell.org@ DNS entry, so an the additional
-- @_mirrors.hackage.haskell.org@ DNS entry in the same SOA doesn't
-- constitute a significant new attack vector anyway.
--
queryBootstrapMirrors :: Verbosity -> URI -> IO [URI]

#if defined(MIN_VERSION_resolv) || defined(MIN_VERSION_windns)
-- use @resolv@ package for performing DNS queries
queryBootstrapMirrors verbosity repoUri
  | Just auth <- uriAuthority repoUri = do
         let mirrorsDnsName = Name (BS.Char8.pack ("_mirrors." ++ uriRegName auth))

         mirrors' <- try $ do
                  txts <- queryTXT mirrorsDnsName
                  evaluate (force $ extractMirrors (map snd txts))

         mirrors <- case mirrors' of
             Left e -> do
                 warn verbosity ("Caught exception during _mirrors lookup:"++
                                 displayException (e :: SomeException))
                 return []
             Right v -> return v

         if null mirrors
         then warn verbosity ("No mirrors found for " ++ show repoUri)
         else do info verbosity ("located " ++ show (length mirrors) ++
                                 " mirrors for " ++ show repoUri ++ " :")
                 forM_ mirrors $ \url -> info verbosity ("- " ++ show url)

         return mirrors

  | otherwise = return []

-- | Extract list of mirrors from 'queryTXT' result
extractMirrors :: [[CharStr]] -> [URI]
extractMirrors txtChunks = mapMaybe (parseURI . snd) . sort $ vals
  where
    vals = [ (kn,v) | CharStr e <- concat txtChunks
                    , Just (k,v) <- [splitRfc1464 (BS.Char8.unpack e)]
                    , Just kn <- [isUrlBase k]
                    ]

----------------------------------------------------------------------------
#else /* !defined(MIN_VERSION_resolv) */
-- use external method via @nslookup@
queryBootstrapMirrors verbosity repoUri
  | Just auth <- uriAuthority repoUri = do
        progdb <- configureAllKnownPrograms verbosity $
                  addKnownProgram nslookupProg emptyProgramDb

        case lookupProgram nslookupProg progdb of
          Nothing -> do
              warn verbosity "'nslookup' tool missing - can't locate mirrors"
              return []

          Just nslookup -> do
              let mirrorsDnsName = "_mirrors." ++ uriRegName auth

              mirrors' <- try $ do
                  out <- getProgramInvocationOutput verbosity $
                         programInvocation nslookup ["-query=TXT", mirrorsDnsName]
                  evaluate (force $ extractMirrors mirrorsDnsName out)

              mirrors <- case mirrors' of
                Left e -> do
                    warn verbosity ("Caught exception during _mirrors lookup:"++
                                    displayException (e :: SomeException))
                    return []
                Right v -> return v

              if null mirrors
              then warn verbosity ("No mirrors found for " ++ show repoUri)
              else do info verbosity ("located " ++ show (length mirrors) ++
                                      " mirrors for " ++ show repoUri ++ " :")
                      forM_ mirrors $ \url -> info verbosity ("- " ++ show url)

              return mirrors

  | otherwise = return []
  where
    nslookupProg = simpleProgram "nslookup"

-- | Extract list of mirrors from @nslookup -query=TXT@ output.
extractMirrors :: String -> String -> [URI]
extractMirrors hostname s0 = mapMaybe (parseURI . snd) . sort $ vals
  where
    vals = [ (kn,v) | (h,ents) <- fromMaybe [] $ parseNsLookupTxt s0
                    , h == hostname
                    , e <- ents
                    , Just (k,v) <- [splitRfc1464 e]
                    , Just kn <- [isUrlBase k]
                    ]

-- | Parse output of @nslookup -query=TXT $HOSTNAME@ tolerantly
parseNsLookupTxt :: String -> Maybe [(String,[String])]
parseNsLookupTxt = go0 [] []
  where
    -- approximate grammar:
    -- <entries> := { <entry> }
    -- (<entry> starts at begin of line, but may span multiple lines)
    -- <entry> := ^ <hostname> TAB "text =" { <qstring> }
    -- <qstring> := string enclosed by '"'s ('\' and '"' are \-escaped)

    -- scan for ^ <word> <TAB> "text ="
    go0 []  _  []                                = Nothing
    go0 res _  []                                = Just (reverse res)
    go0 res _  ('\n':xs)                         = go0 res [] xs
    go0 res lw ('\t':'t':'e':'x':'t':' ':'=':xs) = go1 res (reverse lw) [] (dropWhile isSpace xs)
    go0 res lw (x:xs)                            = go0 res (x:lw) xs

    -- collect at least one <qstring>
    go1 res lw qs ('"':xs) = case qstr "" xs of
      Just (s, xs') -> go1 res lw (s:qs) (dropWhile isSpace xs')
      Nothing       -> Nothing -- bad quoting
    go1 _   _  [] _  = Nothing -- missing qstring
    go1 res lw qs xs = go0 ((lw,reverse qs):res) [] xs

    qstr _   ('\n':_) = Nothing -- We don't support unquoted LFs
    qstr acc ('\\':'\\':cs) = qstr ('\\':acc) cs
    qstr acc ('\\':'"':cs)  = qstr ('"':acc) cs
    qstr acc ('"':cs) = Just (reverse acc, cs)
    qstr acc (c:cs)   = qstr (c:acc) cs
    qstr _   []       = Nothing

#endif
----------------------------------------------------------------------------

-- | Helper used by 'extractMirrors' for extracting @urlbase@ keys from Rfc1464-encoded data
isUrlBase :: String -> Maybe Int
isUrlBase s
  | ".urlbase" `isSuffixOf` s, not (null ns), all isDigit ns = readMaybe ns
  | otherwise = Nothing
  where
    ns = take (length s - 8) s

-- | Split a TXT string into key and value according to RFC1464.
-- Returns 'Nothing' if parsing fails.
splitRfc1464 :: String -> Maybe (String,String)
splitRfc1464 = go ""
  where
    go _ [] = Nothing
    go acc ('`':c:cs) = go (c:acc) cs
    go acc ('=':cs)   = go2 (reverse acc) "" cs
    go acc (c:cs)
      | isSpace c = go acc cs
      | otherwise = go (c:acc) cs

    go2 k acc [] = Just (k,reverse acc)
    go2 _ _   ['`'] = Nothing
    go2 k acc ('`':c:cs) = go2 k (c:acc) cs
    go2 k acc (c:cs) = go2 k (c:acc) cs
