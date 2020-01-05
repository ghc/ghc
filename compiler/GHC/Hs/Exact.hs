{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GHC.Hs.Exact
  (
    ExactPrint(..)
  ) where

-- Module to provide "exact" printing of a GhcPs syntax fragment.
-- This will be the the home of the functionality currently in ghc-exactprint.

-- import GHC.Hs.Extension
-- import GHC.Types.Name.Reader
-- import GHC.Types.SrcLoc
import GHC.Utils.Outputable

-- ---------------------------------------------------------------------

-- | Modeled on Outputable
class ExactPrint a where
  exact :: a -> SDoc

-- ---------------------------------------------------------------------

-- instance ExactPrint (LocatedA RdrName) where
--   exact (L l n) = ppr n

{-
Code in ghc-exactprint

isSymRdr :: GHC.RdrName -> Bool
isSymRdr n = GHC.isSymOcc (GHC.rdrNameOcc n) || rdrName2String n == "."

instance Annotate GHC.RdrName where
  markAST l n = do
    let
      str = rdrName2String n
      isSym = isSymRdr n
      doNormalRdrName = do
        let str' = case str of
              -- TODO: unicode support?
                        "forall" -> if spanLength l == 1 then "∀" else str
                        _ -> str

        let
          markParen :: GHC.AnnKeywordId -> Annotated ()
          markParen pa = do
            if isSym
              then ifInContext (Set.fromList [PrefixOp,PrefixOpDollar])
                                       (mark         pa) -- '('
                                       (markOptional pa)
              else markOptional pa

        markOptional GHC.AnnSimpleQuote
        markParen GHC.AnnOpenP
        unless isSym $ inContext (Set.fromList [InfixOp]) $ markOffset GHC.AnnBackquote 0
        cnt  <- countAnns GHC.AnnVal
        case cnt of
          0 -> markExternal l GHC.AnnVal str'
          1 -> markWithString GHC.AnnVal str'
          _ -> traceM $ "Printing RdrName, more than 1 AnnVal:" ++ showGhc (l,n)
        unless isSym $ inContext (Set.fromList [InfixOp]) $ markOffset GHC.AnnBackquote 1
        markParen GHC.AnnCloseP

    case n of
      GHC.Unqual _ -> doNormalRdrName
      GHC.Qual _ _ -> doNormalRdrName
      GHC.Orig _ _ -> if str == "~"
                        then doNormalRdrName
                        -- then error $ "GHC.orig:(isSym,canParen)=" ++ show (isSym,canParen)
                        else markExternal l GHC.AnnVal str
      -- GHC.Orig _ _ -> markExternal l GHC.AnnVal str
      -- GHC.Orig _ _ -> error $ "GHC.orig:str=[" ++ str ++ "]"
      GHC.Exact n'  -> do
       case str of
         -- Special handling for Exact RdrNames, which are built-in Names
         "[]" -> do
           mark GHC.AnnOpenS  -- '['
           mark GHC.AnnCloseS -- ']'
         "()" -> do
           mark GHC.AnnOpenP  -- '('
           mark GHC.AnnCloseP -- ')'
         ('(':'#':_) -> do
           markWithString GHC.AnnOpen  "(#" -- '(#'
           let cnt = length $ filter (==',') str
           replicateM_ cnt (mark GHC.AnnCommaTuple)
           markWithString GHC.AnnClose  "#)"-- '#)'
         "[::]" -> do
           markWithString GHC.AnnOpen  "[:" -- '[:'
           markWithString GHC.AnnClose ":]" -- ':]'
         "->" -> do
           mark GHC.AnnOpenP -- '('
           mark GHC.AnnRarrow
           mark GHC.AnnCloseP -- ')'
         -- "~#"  -> do
         --   mark GHC.AnnOpenP -- '('
         --   mark GHC.AnnTildehsh
         --   mark GHC.AnnCloseP
         "~"  -> do
           doNormalRdrName
         "*"  -> do
           markExternal l GHC.AnnVal str
         "★"  -> do -- Note: unicode star
           markExternal l GHC.AnnVal str
         ":"  -> do
           -- Note: The OccName for ":" has the following attributes (via occAttributes)
           -- (d, Data DataSym Sym Val )
           -- consDataConName   = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit ":") consDataConKey consDataCon
           doNormalRdrName
           -- trace ("RdrName.checking :" ++ (occAttributes $ GHC.occName n)) doNormalRdrName
         ('(':',':_) -> do
           mark GHC.AnnOpenP
           let cnt = length $ filter (==',') str
           replicateM_ cnt (mark GHC.AnnCommaTuple)
           mark GHC.AnnCloseP -- ')'
         _ -> do
            let isSym' = isSymRdr  (GHC.nameRdrName n')
            when isSym' $ mark GHC.AnnOpenP -- '('
            markWithString GHC.AnnVal str
            when isSym $ mark GHC.AnnCloseP -- ')'
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma `debug` ("AnnComma in RdrName")


-}
