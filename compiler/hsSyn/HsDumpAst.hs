{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Contains a debug function to dump parts of the hsSyn AST. It uses a syb
-- traversal which falls back to displaying based on the constructor name, so
-- can be used to dump anything having a @Data.Data@ instance.

module HsDumpAst (
        -- * Dumping ASTs
        showAstData,
        BlankSrcSpan(..),
    ) where

import Data.Data hiding (Fixity)
import Data.List
import Bag
import BasicTypes
import FastString
import NameSet
import Name
import DataCon
import SrcLoc
import HsSyn
import OccName hiding (occName)
import Var
import Module
import DynFlags
import Outputable hiding (space)

import qualified Data.ByteString as B

data BlankSrcSpan = BlankSrcSpan | NoBlankSrcSpan
                  deriving (Eq,Show)

-- | Show a GHC syntax tree. This parameterised because it is also used for
-- comparing ASTs in ppr roundtripping tests, where the SrcSpan's are blanked
-- out, to avoid comparing locations, only structure
showAstData :: Data a => BlankSrcSpan -> a -> String
showAstData b = showAstData' 0
  where
    showAstData' :: Data a => Int -> a -> String
    showAstData' n =
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
      where generic :: Data a => a -> String
            generic t = indent n ++ "(" ++ showConstr (toConstr t)
                     ++ space (unwords (gmapQ (showAstData' (n+1)) t)) ++ ")"

            space "" = ""
            space s  = ' ':s

            indent i = "\n" ++ replicate i ' '

            string :: String -> String
            string     = normalize_newlines . show

            fastString :: FastString -> String
            fastString = ("{FastString: "++) . (++"}") . normalize_newlines
                       . show

            bytestring :: B.ByteString -> String
            bytestring = normalize_newlines . show

            list l     = indent n ++ "["
                                ++ intercalate "," (map (showAstData' (n+1)) l)
                                ++ "]"

            -- Eliminate word-size dependence
            lit :: HsLit GhcPs -> String
            lit (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            lit (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            lit (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            lit (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            lit l                  = generic l

            litr :: HsLit GhcRn -> String
            litr (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litr (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litr (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litr (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litr l                  = generic l

            litt :: HsLit GhcTc -> String
            litt (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litt (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litt (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litt (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litt l                  = generic l

            numericLit :: String -> Integer -> SourceText -> String
            numericLit tag x s = indent n ++ unwords [ "{" ++ tag
                                                     , generic x
                                                     , generic s ++ "}" ]

            name :: Name -> String
            name       = ("{Name: "++) . (++"}") . showSDocDebug_ . ppr

            occName    = ("{OccName: "++) . (++"}") .  OccName.occNameString

            moduleName :: ModuleName -> String
            moduleName = ("{ModuleName: "++) . (++"}") . showSDoc_ . ppr

            srcSpan :: SrcSpan -> String
            srcSpan ss = case b of
             BlankSrcSpan -> "{ "++ "ss" ++"}"
             NoBlankSrcSpan ->
                             "{ "++ showSDoc_ (hang (ppr ss) (n+2)
                                              -- TODO: show annotations here
                                                    (text "")
                                              )
                          ++"}"

            var  :: Var -> String
            var        = ("{Var: "++) . (++"}") . showSDocDebug_ . ppr

            dataCon :: DataCon -> String
            dataCon    = ("{DataCon: "++) . (++"}") . showSDoc_ . ppr

            bagRdrName:: Bag (Located (HsBind GhcPs)) -> String
            bagRdrName = ("{Bag(Located (HsBind GhcPs)): "++) . (++"}")
                          . list . bagToList

            bagName   :: Bag (Located (HsBind GhcRn)) -> String
            bagName    = ("{Bag(Located (HsBind Name)): "++) . (++"}")
                           . list . bagToList

            bagVar    :: Bag (Located (HsBind GhcTc)) -> String
            bagVar     = ("{Bag(Located (HsBind Var)): "++) . (++"}")
                           . list . bagToList

            nameSet = ("{NameSet: "++) . (++"}") . list . nameSetElemsStable

            fixity :: Fixity -> String
            fixity = ("{Fixity: "++) . (++"}") . showSDoc_ . ppr

            located :: (Data b,Data loc) => GenLocated loc b -> String
            located (L ss a) =
              indent n ++ "("
                ++ case cast ss of
                        Just (s :: SrcSpan) ->
                          srcSpan s
                        Nothing -> "nnnnnnnn"
                      ++ showAstData' (n+1) a
                      ++ ")"

normalize_newlines :: String -> String
normalize_newlines ('\\':'r':'\\':'n':xs) = '\\':'n':normalize_newlines xs
normalize_newlines (x:xs)                 = x:normalize_newlines xs
normalize_newlines []                     = []

showSDoc_ :: SDoc -> String
showSDoc_ = normalize_newlines . showSDoc unsafeGlobalDynFlags

showSDocDebug_ :: SDoc -> String
showSDocDebug_ = normalize_newlines . showSDocDebug unsafeGlobalDynFlags

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
