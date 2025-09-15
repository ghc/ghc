-- | HTML output for documentation package index.

module Ppr049 (
  htmlPage
) where

import Control.Monad
import Data.Char (isAlpha, toUpper)
import Data.List
import Data.Ord
import Data.Time
import Data.Version
import qualified Data.Map as M
import System.FilePath
import System.Locale
import Text.Html

import Distribution.DocIdx.Common
import Distribution.DocIdx.Config
import Distribution.GhcPkgList

-- | Project homepage, for footer.
homePage :: String
homePage = "http://hackage.haskell.org/package/docidx"

-- | Create and render entire page.
htmlPage :: DocIdxCfg -> PackageMap HaddockInfo -> UTCTime -> String
htmlPage config pkgs now = renderHtml [htmlHeader, htmlBody]
  where htmlHeader = header << ((thetitle << pageTitle config) : fav : css)
        fav = thelink ! [rel "shortcut icon", href $ favIcon config] << noHtml
        css = map oneCss (pageCss config)
        oneCss cp = thelink ! [rel "stylesheet",
                               thetype "text/css", href cp] << noHtml
        htmlBody = body << (title' ++ toc ++ secs ++ nowFoot)
          where title' = [h2 << "Local packages with docs"]
                toc = [htmlToc config am]
                secs = concatMap (uncurry htmlPkgsAlpha) $ M.assocs am
                am = alphabetize pkgs
                now' = formatTime defaultTimeLocale rfc822DateFormat now
                nowFoot = [p ! [theclass "toc"] $
                           stringToHtml ("Page rendered " ++ now' ++ " by ")
                           +++ (anchor ! [href homePage] <<
                                          stringToHtml appName)]

-- | An AlphaMap groups packages together by their name's first character.
type AlphaMap = M.Map Char (PackageMap HaddockInfo)

-- | Group packages together by their name's first character.
alphabetize :: PackageMap HaddockInfo -> AlphaMap
alphabetize = foldr addAlpha M.empty
  where addAlpha (n, vs) = M.insertWith (++) c [(n, vs)]
          where c = if isAlpha c' then c' else '\0'
                c' = toUpper $ head n

-- | Generate the table of contents.
htmlToc :: DocIdxCfg -> AlphaMap -> Html
htmlToc config am =
  p ! [theclass "toc"] << tocHtml (alphaItems ++ tocExtras config)
    where tocHtml = intersperse bull . concatMap tocItemHtml
          alphaItems = map (\k -> TocItem [k] ('#':[k])) $ sort $ M.keys am

-- | Render toc elements to HTML.
tocItemHtml :: TocItem -> [Html]
tocItemHtml (TocItem nm path) = [anchor ! [href path] << nm]
tocItemHtml TocSeparator = [mdash]
tocItemHtml TocNewline = [br] -- Hmmm... you still get the bullets?

-- | Render a collection of packages with the same first character.
htmlPkgsAlpha :: Char -> PackageMap HaddockInfo -> [Html]
htmlPkgsAlpha c pm = [heading, packages]
  where heading = h3 ! [theclass "category"] << anchor ! [name [c]] << [c]
        packages = ulist ! [theclass "packages"] <<
                     map (uncurry htmlPkg) pm'
        pm' = sortBy (comparing (map toUpper . fst)) pm

-- | Render a particularly-named package (all versions of it).
htmlPkg :: String -> VersionMap HaddockInfo -> Html
htmlPkg nm vs = li << pvsHtml (flattenPkgVersions nm vs)

-- | Everything we want to know about a particular version of a
-- package, nicely flattened and ready to use.  (Actually, we'd also
-- like to use the synopsis, but this isn't exposed through the Cabal
-- library, sadly.  We could conceivably grab it from the haddock docs
-- (and hackage for packages with no local docs)  but this
-- seems excessive so for now we forget about it.
data PkgVersion = PkgVersion {
    pvName ::String
  , pvSynopsis :: Maybe String
  , pvVersion :: Version
  , pvExposed :: Bool
  , pvHaddocks :: Maybe FilePath
  } deriving (Eq, Ord, Show)

-- | Flatten a given package's various versions into a list of
-- PkgVersion values, which is much nicer to iterate over when
-- building the HTML for this package.
flattenPkgVersions :: String -> VersionMap HaddockInfo -> [PkgVersion]
flattenPkgVersions nm vs = concatMap (uncurry flatten') $ reverse vs
  where flatten' :: Version -> [VersionInfo HaddockInfo] -> [PkgVersion]
        -- We reverse here to put user versions of pkgs before
        -- identically versioned global versions.
        flatten' v = concatMap (uncurry flatten3) . reverse
          where flatten3 :: Bool -> [HaddockInfo] -> [PkgVersion]
                flatten3 ex [] = [PkgVersion nm Nothing v ex Nothing]
                flatten3 ex ps = map (mkPv nm v ex) ps

-- | Construct a PkgVersion from information about a single version of
-- a package.
mkPv :: String -> Version -> Bool -> HaddockInfo -> PkgVersion
mkPv nm v ex Nothing = PkgVersion nm Nothing v ex Nothing
mkPv nm v ex (Just (hp, syn)) = PkgVersion nm (Just syn) v ex (Just hp)

-- | Render the HTML for a list of versions of (we presume) the same
-- package.
pvsHtml :: [PkgVersion] -> Html
pvsHtml pvs = pvHeader (head pvs) +++ spaceHtml +++ pvVersions pvs +++
                pvSyn pvs

-- | Render the "header" part of some package's HTML: name (with link
-- to default version of local docs if available) and hackage link.
pvHeader :: PkgVersion -> [Html]
pvHeader pv = [maybeURL nme (pvHaddocks pv)
              ,spaceHtml
              ,anchor ! [href $ hackagePath pv] << extLinkArrow
              ]
  where nme = if not (pvExposed pv) then "(" ++ nm ++ ")" else nm
        nm = pvName pv

-- | Render HTML linking to the various versions of a package
-- installed, listed by version number only (name is implicit).
pvVersions :: [PkgVersion] -> Html
pvVersions [_] = noHtml -- Don't bother if there's only one version.
pvVersions pvs = stringToHtml "[" +++
                  intersperse comma (map pvOneVer pvs) +++
                  stringToHtml "]"
  where pvOneVer pv = maybeURL (showVersion $ pvVersion pv) (pvHaddocks pv)

-- | Render the synopsis of a package, if present in any of its versions.
pvSyn :: [PkgVersion] -> Html
pvSyn = maybe noHtml (\x -> mdash +++ stringToHtml x) . msum . map pvSynopsis

-- | Render a URL if there's a path; otherwise, just render some text.
-- (Useful in cases where a package is installed but no documentation
-- was found: you'll still get the hackage link.)
maybeURL :: String -> Maybe String -> Html
maybeURL nm Nothing = stringToHtml nm
maybeURL nm (Just path) = anchor ! [href $ joinPath [path, "index.html"]] << nm

-- | Compute the URL to a package's page on hackage.
hackagePath :: PkgVersion -> String
hackagePath pv = "http://hackage.haskell.org/package/" ++ pvTag
  where pvTag = pvName pv ++ "-" ++ showVersion (pvVersion pv)

-- Some primitives.

bull, comma, extLinkArrow, mdash :: Html
bull = primHtml " &bull; "
comma = stringToHtml ", "
extLinkArrow = primHtml "&#x2b08;"
mdash = primHtml " &mdash; "

