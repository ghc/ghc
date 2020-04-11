{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains a debug function to dump parts of the GHC.Hs AST. It uses a syb
-- traversal which falls back to displaying based on the constructor name, so
-- can be used to dump anything having a @Data.Data@ instance.

module GHC.Hs.Dump (
        -- * Dumping ASTs
        showAstData,
        BlankSrcSpan(..),
    ) where

import GHC.Prelude

import Data.Data hiding (Fixity)
import GHC.Data.Bag
import GHC.Types.Basic
import GHC.Data.FastString
import GHC.Types.Name.Set
import GHC.Types.Name
import GHC.Core.DataCon
import GHC.Types.SrcLoc
import GHC.Hs
import GHC.Types.Var
import GHC.Unit.Module
import GHC.Utils.Outputable

import qualified Data.ByteString as B

data BlankSrcSpan = BlankSrcSpan | NoBlankSrcSpan
                  deriving (Eq,Show)

-- | Show a GHC syntax tree. This parameterised because it is also used for
-- comparing ASTs in ppr roundtripping tests, where the SrcSpan's are blanked
-- out, to avoid comparing locations, only structure
showAstData :: Data a => BlankSrcSpan -> a -> SDoc
showAstData b a0 = blankLine $$ showAstData' a0
  where
    showAstData' :: Data a => a -> SDoc
    showAstData' =
      generic
              `ext1Q` list
              `extQ` string `extQ` fastString `extQ` srcSpan
              `extQ` lit `extQ` litr `extQ` litt
              `extQ` bytestring
              `extQ` name `extQ` occName `extQ` moduleName `extQ` var
              `extQ` dataCon
              `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
              `extQ` fixity
              `ext2Q` located

      where generic :: Data a => a -> SDoc
            generic t = parens $ text (showConstr (toConstr t))
                                  $$ vcat (gmapQ showAstData' t)

            string :: String -> SDoc
            string     = text . normalize_newlines . show

            fastString :: FastString -> SDoc
            fastString s = braces $
                            text "FastString: "
                         <> text (normalize_newlines . show $ s)

            bytestring :: B.ByteString -> SDoc
            bytestring = text . normalize_newlines . show

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

            name :: Name -> SDoc
            name nm    = braces $ text "Name: " <> ppr nm

            occName n  =  braces $
                          text "OccName: "
                       <> text (occNameString n)

            moduleName :: ModuleName -> SDoc
            moduleName m = braces $ text "ModuleName: " <> ppr m

            srcSpan :: SrcSpan -> SDoc
            srcSpan ss = case b of
             BlankSrcSpan -> text "{ ss }"
             NoBlankSrcSpan -> braces $ char ' ' <>
                             (hang (ppr ss) 1
                                   -- TODO: show annotations here
                                   (text ""))

            var  :: Var -> SDoc
            var v      = braces $ text "Var: " <> ppr v

            dataCon :: DataCon -> SDoc
            dataCon c  = braces $ text "DataCon: " <> ppr c

            bagRdrName:: Bag (Located (HsBind GhcPs)) -> SDoc
            bagRdrName bg =  braces $
                             text "Bag(Located (HsBind GhcPs)):"
                          $$ (list . bagToList $ bg)

            bagName   :: Bag (Located (HsBind GhcRn)) -> SDoc
            bagName bg  =  braces $
                           text "Bag(Located (HsBind Name)):"
                        $$ (list . bagToList $ bg)

            bagVar    :: Bag (Located (HsBind GhcTc)) -> SDoc
            bagVar bg  =  braces $
                          text "Bag(Located (HsBind Var)):"
                       $$ (list . bagToList $ bg)

            nameSet ns =  braces $
                          text "NameSet:"
                       $$ (list . nameSetElemsStable $ ns)

            fixity :: Fixity -> SDoc
            fixity fx =  braces $
                         text "Fixity: "
                      <> ppr fx

            located :: (Data b,Data loc) => GenLocated loc b -> SDoc
            located (L ss a) = parens $
                   case cast ss of
                        Just (s :: SrcSpan) ->
                          srcSpan s
                        Nothing -> text "nnnnnnnn"
                      $$ showAstData' a

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
