-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Backends.Html.DocMarkup
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Backends.Xhtml.DocMarkup
  ( docToHtml
  , rdrDocToHtml
  , origDocToHtml
  , docToHtmlNoAnchors
  , docElement
  , docSection
  , docSection_
  ) where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import GHC hiding (anchor)
import GHC.Types.Name
import Text.XHtml hiding (name, p, quote)

import Documentation.Haddock.Markup
import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Utils
import Haddock.Doc
  ( combineDocumentation
  , emptyMetaDoc
  , metaConcat
  , metaDocAppend
  )
import Haddock.Types
import Haddock.Utils

parHtmlMarkup
  :: Qualification
  -> Bool
  -> (Bool -> a -> Html)
  -> DocMarkup a Html
parHtmlMarkup qual insertAnchors ppId =
  Markup
    { markupEmpty = noHtml
    , markupString = toHtml
    , markupParagraph = paragraph
    , markupAppend = (+++)
    , markupIdentifier = thecode . ppId insertAnchors
    , markupIdentifierUnchecked = thecode . ppUncheckedLink qual
    , markupModule = \(ModLink m lbl) ->
        let (mdl, ref) = break (== '#') m
            -- Accommodate for old style
            -- foo\#bar anchors
            mdl' = case reverse mdl of
              '\\' : _ -> init mdl
              _ -> mdl
         in ppModuleRef lbl (mkModuleName mdl') ref
    , markupWarning = thediv ! [theclass "warning"]
    , markupEmphasis = emphasize
    , markupBold = strong
    , markupMonospaced = thecode ! [theclass "inline-code"]
    , markupUnorderedList = unordList
    , markupOrderedList = makeOrdList
    , markupDefList = defList
    , markupCodeBlock = pre
    , markupHyperlink = \(Hyperlink url mLabel) ->
        if insertAnchors
          then
            anchor
              ! [href url]
              << fromMaybe (toHtml url) mLabel
          else fromMaybe (toHtml url) mLabel
    , markupAName = \aname ->
        if insertAnchors
          then namedAnchor aname << ""
          else noHtml
    , markupPic = \(Picture uri t) -> image ! ([src uri] ++ fromMaybe [] (return . title <$> t))
    , markupMathInline = \mathjax -> thespan ! [theclass "mathjax"] << toHtml ("\\(" ++ mathjax ++ "\\)")
    , markupMathDisplay = \mathjax -> thespan ! [theclass "mathjax"] << toHtml ("\\[" ++ mathjax ++ "\\]")
    , markupProperty = pre . toHtml
    , markupExample = examplesToHtml
    , markupHeader = \(Header l t) -> makeHeader l t
    , markupTable = \(Table h r) -> makeTable h r
    }
  where
    makeHeader :: Int -> Html -> Html
    makeHeader 1 mkup = h1 mkup
    makeHeader 2 mkup = h2 mkup
    makeHeader 3 mkup = h3 mkup
    makeHeader 4 mkup = h4 mkup
    makeHeader 5 mkup = h5 mkup
    makeHeader 6 mkup = h6 mkup
    makeHeader l _ = error $ "Somehow got a header level `" ++ show l ++ "' in DocMarkup!"

    makeTable :: [TableRow Html] -> [TableRow Html] -> Html
    makeTable hs bs = table (concatHtml (hs' ++ bs'))
      where
        hs'
          | null hs = []
          | otherwise = [thead (concatHtml (map (makeTableRow th) hs))]

        bs' = [tbody (concatHtml (map (makeTableRow td) bs))]

    makeTableRow :: (Html -> Html) -> TableRow Html -> Html
    makeTableRow thr (TableRow cs) = tr (concatHtml (map (makeTableCell thr) cs))

    makeTableCell :: (Html -> Html) -> TableCell Html -> Html
    makeTableCell thr (TableCell i j c) = thr c ! (i' ++ j')
      where
        i' = if i == 1 then [] else [colspan i]
        j' = if j == 1 then [] else [rowspan j]

    examplesToHtml l = pre (concatHtml $ map exampleToHtml l) ! [theclass "screen"]

    exampleToHtml (Example expression result) = htmlExample
      where
        htmlExample = htmlPrompt +++ htmlExpression +++ toHtml (unlines result)
        htmlPrompt = (thecode . toHtml $ ">>> ") ! [theclass "prompt"]
        htmlExpression = (strong . thecode . toHtml $ expression ++ "\n") ! [theclass "userinput"]

    makeOrdList :: HTML a => [(Int, a)] -> Html
    makeOrdList items = olist << map (\(index, a) -> li ! [intAttr "value" index] << a) items

-- | We use this intermediate type to transform the input 'Doc' tree
-- in an arbitrary way before rendering, such as grouping some
-- elements. This is effectively a hack to prevent the 'Doc' type
-- from changing if it is possible to recover the layout information
-- we won't need after the fact.
data Hack a id
  = UntouchedDoc (MetaDoc a id)
  | CollapsingHeader (Header (DocH a id)) (MetaDoc a id) Int (Maybe String)
  | HackAppend (Hack a id) (Hack a id)
  deriving (Eq)

-- | Group things under bold 'DocHeader's together.
toHack
  :: Int
  -- ^ Counter for header IDs which serves to assign
  -- unique identifiers within the comment scope
  -> Maybe String
  -- ^ It is not enough to have unique identifier within the
  -- scope of the comment: if two different comments have the
  -- same ID for headers, the collapse/expand behaviour will act
  -- on them both. This serves to make each header a little bit
  -- more unique. As we can't export things with the same names,
  -- this should work more or less fine: it is in fact the
  -- implicit assumption the collapse/expand mechanism makes for
  -- things like ‘Instances’ boxes.
  -> [MetaDoc a id]
  -> Hack a id
toHack _ _ [] = UntouchedDoc emptyMetaDoc
toHack _ _ [x] = UntouchedDoc x
toHack n nm (MetaDoc{_doc = DocHeader (Header l (DocBold x))} : xs) =
  let
    -- Header with dropped bold
    h = Header l x
    -- Predicate for takeWhile, grab everything including ‘smaller’
    -- headers
    p (MetaDoc{_doc = DocHeader (Header l' _)}) = l' > l
    p _ = True
    -- Stuff ‘under’ this header
    r = takeWhile p xs
    -- Everything else that didn't make it under
    r' = drop (length r) xs
    app y [] = y
    app y ys = HackAppend y (toHack (n + 1) nm ys)
   in
    case r of
      -- No content under this header
      [] -> CollapsingHeader h emptyMetaDoc n nm `app` r'
      -- We got something out, stitch it back together into one chunk
      y : ys -> CollapsingHeader h (foldl metaDocAppend y ys) n nm `app` r'
toHack n nm (x : xs) = HackAppend (UntouchedDoc x) (toHack n nm xs)

-- | Remove ‘top-level’ 'DocAppend's turning them into a flat list.
-- This lends itself much better to processing things in order user
-- might look at them, such as in 'toHack'.
flatten :: MetaDoc a id -> [MetaDoc a id]
flatten MetaDoc{_meta = m, _doc = DocAppend x y} =
  let f z = MetaDoc{_meta = m, _doc = z}
   in flatten (f x) ++ flatten (f y)
flatten x = [x]

-- | Generate the markup needed for collapse to happen. For
-- 'UntouchedDoc' and 'HackAppend' we do nothing more but
-- extract/append the underlying 'Doc' and convert it to 'Html'. For
-- 'CollapsingHeader', we attach extra info to the generated 'Html'
-- that allows us to expand/collapse the content.
hackMarkup :: DocMarkup id Html -> Maybe Package -> Hack (Wrap (ModuleName, OccName)) id -> Html
hackMarkup fmt' currPkg h' =
  let (html, ms) = hackMarkup' fmt' h'
   in html +++ renderMeta fmt' currPkg (metaConcat ms)
  where
    hackMarkup'
      :: DocMarkup id Html
      -> Hack (Wrap (ModuleName, OccName)) id
      -> (Html, [Meta])
    hackMarkup' fmt h = case h of
      UntouchedDoc d -> (markup fmt $ _doc d, [_meta d])
      CollapsingHeader (Header lvl titl) par n nm ->
        let id_ = makeAnchorId $ "ch:" ++ fromMaybe "noid:" nm ++ show n
            col' = collapseControl id_ "subheading"
            summary = thesummary ! [theclass "hide-when-js-enabled"] << "Expand"
            instTable contents = collapseDetails id_ DetailsClosed (summary +++ contents)
            lvs = zip [1 ..] [h1, h2, h3, h4, h5, h6]
            getHeader = fromMaybe caption (lookup lvl lvs)
            subCaption = getHeader ! col' << markup fmt titl
         in ((subCaption +++) . instTable $ markup fmt (_doc par), [_meta par])
      HackAppend d d' ->
        let (x, m) = hackMarkup' fmt d
            (y, m') = hackMarkup' fmt d'
         in (markupAppend fmt x y, m ++ m')

renderMeta :: DocMarkup id Html -> Maybe Package -> Meta -> Html
renderMeta fmt currPkg m =
  maybe noHtml (renderMetaSince fmt currPkg) (_metaSince m)

renderMetaSince :: DocMarkup id Html -> Maybe Package -> MetaSince -> Html
renderMetaSince fmt currPkg (MetaSince{sincePackage = pkg, sinceVersion = ver}) =
  markupParagraph fmt . markupEmphasis fmt . toHtml $
    "Since: " ++ formatPkgMaybe pkg ++ formatVersion ver
  where
    formatVersion v = concat . intersperse "." $ map show v
    formatPkgMaybe (Just p) | Just p /= currPkg = p ++ "-"
    formatPkgMaybe _ = ""

-- | Goes through 'hackMarkup' to generate the 'Html' rather than
-- skipping straight to 'markup': this allows us to employ XHtml
-- specific hacks to the tree first.
markupHacked
  :: DocMarkup (Wrap id) Html
  -> Maybe Package -- this package
  -> Maybe String
  -> MDoc id
  -> Html
markupHacked fmt currPkg n = hackMarkup fmt currPkg . toHack 0 n . flatten

-- If the doc is a single paragraph, don't surround it with <P> (this causes
-- ugly extra whitespace with some browsers).  FIXME: Does this still apply?
docToHtml
  :: Maybe String
  -- ^ Name of the thing this doc is for. See
  -- comments on 'toHack' for details.
  -> Maybe Package
  -- ^ Current package
  -> Qualification
  -> MDoc DocName
  -> Html
docToHtml n pkg qual = markupHacked fmt pkg n . cleanup
  where
    fmt = parHtmlMarkup qual True (ppWrappedDocName qual Raw)

-- | Same as 'docToHtml' but it doesn't insert the 'anchor' element
-- in links. This is used to generate the Contents box elements.
docToHtmlNoAnchors
  :: Maybe String
  -- ^ See 'toHack'
  -> Maybe Package
  -- ^ Current package
  -> Qualification
  -> MDoc DocName
  -> Html
docToHtmlNoAnchors n pkg qual = markupHacked fmt pkg n . cleanup
  where
    fmt = parHtmlMarkup qual False (ppWrappedDocName qual Raw)

origDocToHtml :: Maybe Package -> Qualification -> MDoc Name -> Html
origDocToHtml pkg qual = markupHacked fmt pkg Nothing . cleanup
  where
    fmt = parHtmlMarkup qual True (const (ppWrappedName Raw))

rdrDocToHtml :: Maybe Package -> Qualification -> MDoc RdrName -> Html
rdrDocToHtml pkg qual = markupHacked fmt pkg Nothing . cleanup
  where
    fmt = parHtmlMarkup qual True (const (ppRdrName . unwrap))

docElement :: (Html -> Html) -> Html -> Html
docElement el content_ =
  if isNoHtml content_
    then el ! [theclass "doc empty"] << spaceHtml
    else el ! [theclass "doc"] << content_

docSection
  :: Maybe Name
  -- ^ Name of the thing this doc is for
  -> Maybe Package
  -- ^ Current package
  -> Qualification
  -> Documentation DocName
  -> Html
docSection n pkg qual =
  maybe noHtml (docSection_ n pkg qual) . combineDocumentation

docSection_
  :: Maybe Name
  -- ^ Name of the thing this doc is for
  -> Maybe Package
  -- ^ Current package
  -> Qualification
  -> MDoc DocName
  -> Html
docSection_ n pkg qual =
  (docElement thediv <<) . docToHtml (getOccString <$> n) pkg qual

cleanup :: MDoc a -> MDoc a
cleanup = overDoc (markup fmtUnParagraphLists)
  where
    -- If there is a single paragraph, then surrounding it with <P>..</P>
    -- can add too much whitespace in some browsers (eg. IE).  However if
    -- we have multiple paragraphs, then we want the extra whitespace to
    -- separate them.  So we catch the single paragraph case and transform it
    -- here. We don't do this in code blocks as it eliminates line breaks.
    unParagraph :: Doc a -> Doc a
    unParagraph (DocParagraph d) = d
    unParagraph doc = doc

    fmtUnParagraphLists :: DocMarkup (Wrap a) (Doc a)
    fmtUnParagraphLists =
      idMarkup
        { markupUnorderedList = DocUnorderedList . map unParagraph
        , markupOrderedList = DocOrderedList . map (\(index, a) -> (index, unParagraph a))
        }
