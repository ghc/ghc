-- | @since 1.4.5
module Documentation.Haddock.Markup (
    markup
  , idMarkup
  ) where

import Documentation.Haddock.Types

markup :: DocMarkupH mod id a -> DocH mod id -> a
markup m DocEmpty                    = markupEmpty m
markup m (DocAppend d1 d2)           = markupAppend m (markup m d1) (markup m d2)
markup m (DocString s)               = markupString m s
markup m (DocParagraph d)            = markupParagraph m (markup m d)
markup m (DocIdentifier x)           = markupIdentifier m x
markup m (DocIdentifierUnchecked x)  = markupIdentifierUnchecked m x
markup m (DocModule mod0)            = markupModule m mod0
markup m (DocWarning d)              = markupWarning m (markup m d)
markup m (DocEmphasis d)             = markupEmphasis m (markup m d)
markup m (DocBold d)                 = markupBold m (markup m d)
markup m (DocMonospaced d)           = markupMonospaced m (markup m d)
markup m (DocUnorderedList ds)       = markupUnorderedList m (map (markup m) ds)
markup m (DocOrderedList ds)         = markupOrderedList m (map (markup m) ds)
markup m (DocDefList ds)             = markupDefList m (map (markupPair m) ds)
markup m (DocCodeBlock d)            = markupCodeBlock m (markup m d)
markup m (DocHyperlink l)            = markupHyperlink m l
markup m (DocAName ref)              = markupAName m ref
markup m (DocPic img)                = markupPic m img
markup m (DocMathInline mathjax)     = markupMathInline m mathjax
markup m (DocMathDisplay mathjax)    = markupMathDisplay m mathjax
markup m (DocProperty p)             = markupProperty m p
markup m (DocExamples e)             = markupExample m e
markup m (DocHeader (Header l t))    = markupHeader m (Header l (markup m t))
markup m (DocTable (Table h b))      = markupTable m (Table (map (fmap (markup m)) h) (map (fmap (markup m)) b))

markupPair :: DocMarkupH mod id a -> (DocH mod id, DocH mod id) -> (a, a)
markupPair m (a,b) = (markup m a, markup m b)

-- | The identity markup
idMarkup :: DocMarkupH mod id (DocH mod id)
idMarkup = Markup {
  markupEmpty                = DocEmpty,
  markupString               = DocString,
  markupParagraph            = DocParagraph,
  markupAppend               = DocAppend,
  markupIdentifier           = DocIdentifier,
  markupIdentifierUnchecked  = DocIdentifierUnchecked,
  markupModule               = DocModule,
  markupWarning              = DocWarning,
  markupEmphasis             = DocEmphasis,
  markupBold                 = DocBold,
  markupMonospaced           = DocMonospaced,
  markupUnorderedList        = DocUnorderedList,
  markupOrderedList          = DocOrderedList,
  markupDefList              = DocDefList,
  markupCodeBlock            = DocCodeBlock,
  markupHyperlink            = DocHyperlink,
  markupAName                = DocAName,
  markupPic                  = DocPic,
  markupMathInline           = DocMathInline,
  markupMathDisplay          = DocMathDisplay,
  markupProperty             = DocProperty,
  markupExample              = DocExamples,
  markupHeader               = DocHeader,
  markupTable                = DocTable
  }
