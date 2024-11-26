{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty
    ( module Prettyprinter
    , Doc
    , mkMsg
    , Severity(..)
    , severityIcon
    , bulletList
    , ppCommit
    , ppPackage
    , ppVersion
    , ppHeading
    , putDoc
    ) where

import Data.Version
import Package
import Prettyprinter hiding (Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal
import Distribution.Types.PackageName qualified as C

type Doc = PP.Doc AnsiStyle

ppPackage :: Package -> Doc
ppPackage =
    annotate (color Green) . pretty . C.unPackageName . pkgName

ppVersion :: Version -> Doc
ppVersion v =
    annotate (color Blue) $ pretty $ showVersion v

ppCommit :: Doc -> Doc
ppCommit =
    annotate (color Blue)

ppHeading :: Doc -> Doc
ppHeading =
    annotate bold . ("#" <+>)

bullet :: Doc
bullet = "‣"

bulletList :: [Doc] -> Doc
bulletList xs = vcat [ " " <> bullet <+> align x | x <- xs ]

data Severity = Info | Warning | Error

severityIcon :: Severity -> Doc
severityIcon Info    = annotate (color Blue) "ℹ" -- "🔵"
severityIcon Warning = "🟡"
severityIcon Error   = annotate (color Red) "✗" -- "🔴"

mkMsg :: Severity -> Doc -> Doc
mkMsg s msg = severityIcon s <+> msg
