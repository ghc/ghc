{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- | Contains a debug function to dump parts of the GHC.Hs AST. It uses a syb
-- traversal which falls back to displaying based on the constructor name, so
-- can be used to dump anything having a @Data.Data@ instance.

module GHC.Hs.Dump (
        -- * Dumping ASTs
        showAstData,
        showAstDataFull,
        BlankSrcSpan(..),
        BlankEpAnnotations(..),
    ) where

import GHC.Prelude

import GHC.Hs

import GHC.Core.DataCon

import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Types.Name.Set
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Types.SourceText
import GHC.Utils.Outputable

import Data.Data hiding (Fixity)
import qualified Data.ByteString as B
import GHC.TypeLits

-- | Should source spans be removed from output.
data BlankSrcSpan = BlankSrcSpan | BlankSrcSpanFile | NoBlankSrcSpan
                  deriving (Eq,Show)

-- | Should EpAnnotations be removed from output.
data BlankEpAnnotations = BlankEpAnnotations | NoBlankEpAnnotations
                  deriving (Eq,Show)

-- | Show the full AST as the compiler sees it.
showAstDataFull :: Data a => a -> SDoc
showAstDataFull = showAstData NoBlankSrcSpan NoBlankEpAnnotations

-- | Show a GHC syntax tree. This parameterised because it is also used for
-- comparing ASTs in ppr roundtripping tests, where the SrcSpan's are blanked
-- out, to avoid comparing locations, only structure
showAstData :: Data a => BlankSrcSpan -> BlankEpAnnotations -> a -> SDoc
showAstData bs ba a0 = blankLine $$ showAstData' a0
  where
    showAstData' :: Data a => a -> SDoc
    showAstData' =
      generic
              `ext1Q` list
              `extQ` list_addEpAnn
              `extQ` list_epaLocation
              `extQ` list_epTokenOpenP
              `extQ` list_epTokenCloseP
              `extQ` string `extQ` fastString `extQ` srcSpan `extQ` realSrcSpan
              `extQ` annotationModule
              `extQ` annotationGrhsAnn
              `extQ` annotationAnnList
              `extQ` annotationEpAnnImportDecl
              `extQ` annotationNoEpAnns
              `extQ` annotationExprBracket
              `extQ` annotationTypedBracket
              `extQ` addEpAnn
              `extQ` epTokenOC
              `extQ` epTokenCC
              `extQ` epTokenInstance
              `extQ` epTokenForall
              `extQ` annParen
              `extQ` annClassDecl
              `extQ` annSynDecl
              `extQ` annDataDefn
              `extQ` annFamilyDecl
              `extQ` annClsInstDecl
              `extQ` lit `extQ` litr `extQ` litt
              `extQ` sourceText
              `extQ` deltaPos
              `extQ` epaLocation
              `extQ` maybe_epaLocation
              `extQ` bytestring
              `extQ` name `extQ` occName `extQ` moduleName `extQ` var
              `extQ` dataCon
              `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
              `ext2Q` located
              `extQ` srcSpanAnnA
              `extQ` srcSpanAnnL
              `extQ` srcSpanAnnP
              `extQ` srcSpanAnnC
              `extQ` srcSpanAnnN

      where generic :: Data a => a -> SDoc
            generic t = parens $ text (showConstr (toConstr t))
                                  $$ vcat (gmapQ showAstData' t)

            string :: String -> SDoc
            string     = text . normalize_newlines . show

            fastString :: FastString -> SDoc
            fastString s = braces $
                            text "FastString:"
                        <+> text (normalize_newlines . show $ s)

            bytestring :: B.ByteString -> SDoc
            bytestring = text . normalize_newlines . show

            list_addEpAnn :: [AddEpAnn] -> SDoc
            list_addEpAnn ls = case ba of
              BlankEpAnnotations -> parens
                                       $ text "blanked:" <+> text "[AddEpAnn]"
              NoBlankEpAnnotations -> list ls

            list_epaLocation :: [EpaLocation] -> SDoc
            list_epaLocation ls = case ba of
              BlankEpAnnotations -> parens
                                       $ text "blanked:" <+> text "[EpaLocation]"
              NoBlankEpAnnotations -> list ls

            list_epTokenOpenP :: [EpToken "("] -> SDoc
            list_epTokenOpenP ls = case ba of
              BlankEpAnnotations -> parens
                                       $ text "blanked:" <+> text "[EpToken \"(\"]"
              NoBlankEpAnnotations -> list ls

            list_epTokenCloseP :: [EpToken ")"] -> SDoc
            list_epTokenCloseP ls = case ba of
              BlankEpAnnotations -> parens
                                       $ text "blanked:" <+> text "[EpToken \"(\"]"
              NoBlankEpAnnotations -> list ls

            list []    = brackets empty
            list [x]   = brackets (showAstData' x)
            list (x1 : x2 : xs) =  (text "[" <> showAstData' x1)
                                $$ go x2 xs
              where
                go y [] = text "," <> showAstData' y <> text "]"
                go y1 (y2 : ys) = (text "," <> showAstData' y1) $$ go y2 ys

            -- Eliminate word-size dependence
            lit :: HsLit GhcPs -> SDoc
            lit (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            lit (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            lit (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            lit (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            lit l                  = generic l

            litr :: HsLit GhcRn -> SDoc
            litr (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litr (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litr (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litr (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litr l                  = generic l

            litt :: HsLit GhcTc -> SDoc
            litt (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litt (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litt (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litt (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litt l                  = generic l

            numericLit :: String -> Integer -> SourceText -> SDoc
            numericLit tag x s = braces $ hsep [ text tag
                                               , generic x
                                               , generic s ]

            sourceText :: SourceText -> SDoc
            sourceText NoSourceText = case bs of
              BlankSrcSpan -> parens $ text "SourceText" <+> text "blanked"
              _            -> parens $ text "NoSourceText"
            sourceText (SourceText src) = case bs of
              BlankSrcSpan     -> parens $ text "SourceText" <+> text "blanked"
              _                -> parens $ text "SourceText" <+> ftext src

            epaLocation :: EpaLocation -> SDoc
            epaLocation (EpaSpan s) = parens $ text "EpaSpan" <+> srcSpan s
            epaLocation (EpaDelta s d cs) = case ba of
              NoBlankEpAnnotations -> parens $ text "EpaDelta" <+> srcSpan s <+> deltaPos d <+> showAstData' cs
              BlankEpAnnotations -> parens $ text "EpaDelta" <+> srcSpan s <+> deltaPos d <+> text "blanked"

            maybe_epaLocation :: Maybe EpaLocation -> SDoc
            maybe_epaLocation ml = case ba of
              NoBlankEpAnnotations -> case ml of
                Nothing -> parens $ text "Nothing"
                Just l -> parens (text "Just" $$ showAstData' l)
              BlankEpAnnotations -> parens $ text "Maybe EpaLocation:" <+> text "blanked"

            deltaPos :: DeltaPos -> SDoc
            deltaPos (SameLine c) = parens $ text "SameLine" <+> ppr c
            deltaPos (DifferentLine l c) = parens $ text "DifferentLine" <+> ppr l <+> ppr c

            name :: Name -> SDoc
            name nm    = braces $ text "Name:" <+> ppr nm

            occName n  =  braces $
                          text "OccName:"
                      <+> ftext (occNameFS n)

            moduleName :: ModuleName -> SDoc
            moduleName m = braces $ text "ModuleName:" <+> ppr m

            srcSpan :: SrcSpan -> SDoc
            srcSpan ss = case bs of
             BlankSrcSpan -> text "{ ss }"
             NoBlankSrcSpan -> braces $ char ' ' <> (ppr ss) <> char ' '
             BlankSrcSpanFile -> braces $ char ' ' <> (pprUserSpan False ss) <> char ' '

            realSrcSpan :: RealSrcSpan -> SDoc
            realSrcSpan ss = case bs of
             BlankSrcSpan -> text "{ ss }"
             NoBlankSrcSpan -> braces $ char ' ' <> (ppr ss) <> char ' '
             BlankSrcSpanFile -> braces $ char ' ' <> (pprUserRealSpan False ss) <> char ' '

            annParen :: AnnParen -> SDoc
            annParen (AnnParen a o c) = case ba of
             BlankEpAnnotations -> parens $ text "blanked:" <+> text "AnnParen"
             NoBlankEpAnnotations ->
              parens $ text "AnnParen"
                        $$ vcat [ppr a, epaLocation o, epaLocation c]

            annClassDecl :: AnnClassDecl -> SDoc
            annClassDecl (AnnClassDecl c ops cps v w oc cc s) = case ba of
             BlankEpAnnotations -> parens $ text "blanked:" <+> text "AnnClassDecl"
             NoBlankEpAnnotations ->
              parens $ text "AnnClassDecl"
                        $$ vcat [showAstData' c, showAstData' ops, showAstData' cps,
                                 showAstData' v, showAstData' w, showAstData' oc,
                                 showAstData' cc, showAstData' s]

            annSynDecl :: AnnSynDecl -> SDoc
            annSynDecl (AnnSynDecl ops cps t e) = case ba of
             BlankEpAnnotations -> parens $ text "blanked:" <+> text "AnnSynDecl"
             NoBlankEpAnnotations ->
              parens $ text "AnnSynDecl"
                        $$ vcat [showAstData' ops, showAstData' cps,
                                 showAstData' t, showAstData' e]

            annDataDefn :: AnnDataDefn -> SDoc
            annDataDefn (AnnDataDefn a b c d e f g h i j k) = case ba of
             BlankEpAnnotations -> parens $ text "blanked:" <+> text "AnnDataDefn"
             NoBlankEpAnnotations ->
              parens $ text "AnnDataDefn"
                        $$ vcat [showAstData' a, showAstData' b, showAstData' c,
                                 showAstData' d, showAstData' e, showAstData' f,
                                 showAstData' g, showAstData' h, showAstData' i,
                                 showAstData' j, showAstData' k]

            annFamilyDecl :: AnnFamilyDecl -> SDoc
            annFamilyDecl (AnnFamilyDecl a b c d e f g h i j k l) = case ba of
             BlankEpAnnotations -> parens $ text "blanked:" <+> text "AnnFamilyDecl"
             NoBlankEpAnnotations ->
              parens $ text "AnnFamilyDecl"
                        $$ vcat [showAstData' a, showAstData' b, showAstData' c,
                                 showAstData' d, showAstData' e, showAstData' f,
                                 showAstData' g, showAstData' h, showAstData' i,
                                 showAstData' j, showAstData' k, showAstData' l]

            annClsInstDecl :: AnnClsInstDecl -> SDoc
            annClsInstDecl (AnnClsInstDecl a b c d e) = case ba of
             BlankEpAnnotations -> parens $ text "blanked:" <+> text "AnnFamilyDecl"
             NoBlankEpAnnotations ->
              parens $ text "AnnClsInstDecl"
                        $$ vcat [showAstData' a, showAstData' b, showAstData' c,
                                 showAstData' d, showAstData' e]


            addEpAnn :: AddEpAnn -> SDoc
            addEpAnn (AddEpAnn a s) = case ba of
             BlankEpAnnotations -> parens
                                      $ text "blanked:" <+> text "AddEpAnn"
             NoBlankEpAnnotations ->
              parens $ text "AddEpAnn" <+> ppr a <+> epaLocation s

            annotationExprBracket :: BracketAnn (EpUniToken "[|" "⟦") (EpToken "[e|") -> SDoc
            annotationExprBracket = annotationBracket

            annotationTypedBracket :: BracketAnn (EpToken "[||") (EpToken "[e||") -> SDoc
            annotationTypedBracket = annotationBracket

            annotationBracket ::forall n h .(Data n, Data h, Typeable n, Typeable h)
              => BracketAnn n h -> SDoc
            annotationBracket a = case ba of
             BlankEpAnnotations -> parens
                                      $ text "blanked:" <+> text "BracketAnn"
             NoBlankEpAnnotations ->
              parens $ case a of
                BracketNoE  t -> text "BracketNoE"  <+> showAstData' t
                BracketHasE t -> text "BracketHasE" <+> showAstData' t

            epTokenOC :: EpToken "{" -> SDoc
            epTokenOC  = epToken'

            epTokenCC :: EpToken "}" -> SDoc
            epTokenCC = epToken'

            epTokenInstance :: EpToken "instance" -> SDoc
            epTokenInstance = epToken'

            epTokenForall :: TokForall -> SDoc
            epTokenForall = epUniToken'

            epToken' :: KnownSymbol sym => EpToken sym -> SDoc
            epToken' (EpTok s) = case ba of
             BlankEpAnnotations -> parens
                                      $ text "blanked:" <+> text "EpToken"
             NoBlankEpAnnotations ->
              parens $ text "EpTok" <+> epaLocation s
            epToken' NoEpTok = case ba of
             BlankEpAnnotations -> parens
                                      $ text "blanked:" <+> text "EpToken"
             NoBlankEpAnnotations ->
              parens $ text "NoEpTok"

            epUniToken' :: EpUniToken sym1 sym2 -> SDoc
            epUniToken' (EpUniTok s f) = case ba of
             BlankEpAnnotations -> parens
                                      $ text "blanked:" <+> text "EpUniToken"
             NoBlankEpAnnotations ->
              parens $ text "EpUniTok" <+> epaLocation s <+> ppr f
            epUniToken' NoEpUniTok = case ba of
             BlankEpAnnotations -> parens
                                      $ text "blanked:" <+> text "EpUniToken"
             NoBlankEpAnnotations ->
              parens $ text "NoEpUniTok"


            var  :: Var -> SDoc
            var v      = braces $ text "Var:" <+> ppr v

            dataCon :: DataCon -> SDoc
            dataCon c  = braces $ text "DataCon:" <+> ppr c

            bagRdrName:: Bag (LocatedA (HsBind GhcPs)) -> SDoc
            bagRdrName bg =  braces $
                             text "Bag(LocatedA (HsBind GhcPs)):"
                          $$ (list . bagToList $ bg)

            bagName   :: Bag (LocatedA (HsBind GhcRn)) -> SDoc
            bagName bg  =  braces $
                           text "Bag(LocatedA (HsBind Name)):"
                        $$ (list . bagToList $ bg)

            bagVar    :: Bag (LocatedA (HsBind GhcTc)) -> SDoc
            bagVar bg  =  braces $
                          text "Bag(LocatedA (HsBind Var)):"
                       $$ (list . bagToList $ bg)

            nameSet ns =  braces $
                          text "NameSet:"
                       $$ (list . nameSetElemsStable $ ns)

            located :: (Data a, Data b) => GenLocated a b -> SDoc
            located (L ss a)
              = parens (text "L"
                        $$ vcat [showAstData' ss, showAstData' a])


            -- -------------------------

            annotationModule :: EpAnn AnnsModule -> SDoc
            annotationModule = annotation' (text "EpAnn AnnsModule")

            annotationGrhsAnn :: EpAnn GrhsAnn -> SDoc
            annotationGrhsAnn = annotation' (text "EpAnn GrhsAnn")

            annotationAnnList :: EpAnn AnnList -> SDoc
            annotationAnnList = annotation' (text "EpAnn AnnList")

            annotationEpAnnImportDecl :: EpAnn EpAnnImportDecl -> SDoc
            annotationEpAnnImportDecl = annotation' (text "EpAnn EpAnnImportDecl")

            annotationNoEpAnns :: EpAnn NoEpAnns -> SDoc
            annotationNoEpAnns = annotation' (text "EpAnn NoEpAnns")

            annotation' :: forall a .(Data a, Typeable a)
                       => SDoc -> EpAnn a -> SDoc
            annotation' tag anns = case ba of
             BlankEpAnnotations -> parens (text "blanked:" <+> tag)
             NoBlankEpAnnotations -> parens $ text (showConstr (toConstr anns))
                                               $$ vcat (gmapQ showAstData' anns)

            -- -------------------------

            srcSpanAnnA :: EpAnn AnnListItem -> SDoc
            srcSpanAnnA = locatedAnn'' (text "SrcSpanAnnA")

            srcSpanAnnL :: EpAnn AnnList -> SDoc
            srcSpanAnnL = locatedAnn'' (text "SrcSpanAnnL")

            srcSpanAnnP :: EpAnn AnnPragma -> SDoc
            srcSpanAnnP = locatedAnn'' (text "SrcSpanAnnP")

            srcSpanAnnC :: EpAnn AnnContext -> SDoc
            srcSpanAnnC = locatedAnn'' (text "SrcSpanAnnC")

            srcSpanAnnN :: EpAnn NameAnn -> SDoc
            srcSpanAnnN = locatedAnn'' (text "SrcSpanAnnN")

            locatedAnn'' :: forall a. (Typeable a, Data a)
              => SDoc -> EpAnn a -> SDoc
            locatedAnn'' tag ss = parens $
              case cast ss of
                Just (ann :: EpAnn a) ->
                  case ba of
                    BlankEpAnnotations
                      -> parens (text "blanked:" <+> tag)
                    NoBlankEpAnnotations
                      -> text (showConstr (toConstr ann))
                                          $$ vcat (gmapQ showAstData' ann)
                Nothing -> text "locatedAnn:unmatched" <+> tag
                           <+> (parens $ text (showConstr (toConstr ss)))


normalize_newlines :: String -> String
normalize_newlines ('\\':'r':'\\':'n':xs) = '\\':'n':normalize_newlines xs
normalize_newlines (x:xs)                 = x:normalize_newlines xs
normalize_newlines []                     = []

{-
************************************************************************
*                                                                      *
* Copied from syb
*                                                                      *
************************************************************************
-}


-- | The type constructor for queries
newtype Q q x = Q { unQ :: x -> q }

-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)

-- | Type extension of queries for type constructors
ext1Q :: (Data d, Typeable t)
      => (d -> q)
      -> (forall e. Data e => t e -> q)
      -> d -> q
ext1Q def ext = unQ ((Q def) `ext1` (Q ext))


-- | Type extension of queries for type constructors
ext2Q :: (Data d, Typeable t)
      => (d -> q)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> q)
      -> d -> q
ext2Q def ext = unQ ((Q def) `ext2` (Q ext))

-- | Flexible type extension
ext1 :: (Data a, Typeable t)
     => c a
     -> (forall d. Data d => c (t d))
     -> c a
ext1 def ext = maybe def id (dataCast1 ext)



-- | Flexible type extension
ext2 :: (Data a, Typeable t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2 def ext = maybe def id (dataCast2 ext)
