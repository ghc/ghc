
module DPH.Core.Pretty
        ( module DPH.Base.Pretty
        , pprModGuts
        , pprTopBinds)
where
import DPH.Base.Pretty
import HscTypes
import Avail
import CoreSyn
import Type
import Coercion
import Var
import Name
import OccName
import DataCon
import Literal
import Id
import Unique
import qualified UniqDFM as UDFM

-- Guts -----------------------------------------------------------------------
pprModGuts :: ModGuts -> Doc
pprModGuts guts
 = vcat
 [ text "Exports:"
        <+> ppr (mg_exports guts)
 , empty

 , text "VectInfo:"
        <+> ppr (mg_vect_info guts)
 , empty

 , pprTopBinds $ mg_binds guts]


-- | An AvailInfo carries an exported name.
instance Pretty AvailInfo where
 ppr aa
  = case aa of
        Avail n         -> ppr n
        AvailTC n _     -> ppr n


-- | The VectInfo maps names to their vectorised versions.
instance Pretty VectInfo where
 ppr vi
  = ppr $ UDFM.eltsUDFM (vectInfoVar vi)


-- Top Binds ------------------------------------------------------------------
pprTopBinds :: Pretty a => [Bind a] -> Doc
pprTopBinds binds
        = vcat $ map pprTopBind binds

pprTopBind  :: Pretty a => Bind a -> Doc
pprTopBind (NonRec binder expr)
  =    pprBinding (binder, expr)
  <$$> empty

pprTopBind (Rec [])
  = text "Rec { }"

pprTopBind (Rec bb@(b:bs))
  = vcat
  [ text "Rec {"
  , vcat [empty <$$> pprBinding b | b <- bb]
  , text "end Rec }"
  , empty ]


-- Binding --------------------------------------------------------------------
pprBinding :: Pretty a => (a, Expr a) -> Doc
pprBinding (binder, x)
        =   ppr binder
        <+> breakWhen (not $ isSimpleX x)
        <+> equals <+> align (ppr x)



-- Expr -----------------------------------------------------------------------
instance Pretty a => Pretty (Expr a) where
 pprPrec d xx
  = case xx of
        Var  ident
         -> pprBound ident

        -- Discard types and coersions
        Type _          -> empty
        Coercion _      -> empty

        -- Literals.
        Lit ll          -> ppr ll

        -- Suppress Casts completely.
        Cast x _co
         -> pprPrec d x

        -- Abstractions.
        Lam{}
         -> pprParen' (d > 2)
         $  let (bndrs, body) = collectBinders xx
            in  text "\\" <> sep (map ppr bndrs)
                 <> text "."
                 <> (nest 2
                        $ (breakWhen $ not $ isSimpleX body)
                         <> ppr body)

        -- Applications.
        App x1 x2
         |  isTypeArg x2
         -> pprPrec d x1

         |  otherwise
         -> pprParen' (d > 10)
         $  ppr x1
                <> nest 2 (breakWhen (not $ isSimpleX x2)
                                <> pprPrec 11 x2)

        -- Destructors.
        Case x1 var ty [(con, binds, x2)]
         -> pprParen' (d > 2)
         $  text "let"
                <+> (fill 12 (ppr con <+> hsep (map ppr binds)))
--                <>  breakWhen (not $ isSimpleX x1)
                        <+>  text "<-"
                        <+> ppr x1
                        <+> text "in"
                <$$> ppr x2

        Case x1 var ty alts
         -> pprParen' (d > 2)
         $  (nest 2
                $ text "case" <+> ppr x1 <+> text "of"
                <+> ppr var
                <+> lbrace <> line
                        <> vcat (punctuate semi $ map pprAlt alts))
         <>  line <> rbrace

        -- Binding.
        Let (NonRec b x1) x2
         -> pprParen' (d > 2)
         $  text "let"
                <+> fill 12 (ppr b)
                <+> equals
                <+> ppr x1
                <+> text "in"
                <$$> ppr x2

        Let (Rec bxs) x2
          -> pprParen' (d > 2)
          $  text "letrec {"
                <+> vcat [ fill 12 (ppr b)
                                 <+> equals
                                 <+> ppr x
                         | (b, x) <- bxs]
                <+> text "} in"
                <$$> ppr x2

        _ -> text "DUNNO"


-- Alt ------------------------------------------------------------------------
pprAlt :: Pretty a => (AltCon, [a], Expr a) -> Doc
pprAlt (con, binds, x)
        = ppr con <+> (hsep $ map ppr binds)
        <+> nest 1 (line <> nest 3 (text "->" <+> ppr x))

instance Pretty AltCon where
 ppr con
  = case con of
        DataAlt con     -> ppr con
        LitAlt  lit     -> ppr lit
        DEFAULT         -> text "_"


-- | Pretty print bound occurrences of an identifier
pprBound :: Id -> Doc
pprBound i
        -- Suppress uniqueids from primops, dictionary functions and data constructors
        -- These are unlikely to have conflicting base names.
        |   isPrimOpId i || isDFunId i || isDataConWorkId i
        =  ppr (idName i)

        | otherwise
        = ppr (idName i) <> text "_" <> text (show $ idUnique i)



-- Literal --------------------------------------------------------------------
instance Pretty Literal where
 ppr _  = text "<LITERAL>"


-- Type -----------------------------------------------------------------------
instance Pretty Type where
 ppr _  = empty


-- Coercion -------------------------------------------------------------------
instance Pretty Coercion where
 ppr _  = empty


-- Names ----------------------------------------------------------------------
instance Pretty CoreBndr where
 ppr bndr
        =  ppr (idName bndr)
        <> text "_"
        <> text (show $ idUnique bndr)


instance Pretty DataCon where
 ppr con
        = ppr (dataConName con)

instance Pretty Name where
 ppr name
        = ppr (nameOccName name)

instance Pretty OccName where
 ppr occName
        = text (occNameString occName)



-- Utils ----------------------------------------------------------------------
breakWhen :: Bool -> Doc
breakWhen True   = line
breakWhen False  = space


isSimpleX :: Expr a -> Bool
isSimpleX xx
 = case xx of
        Var{}           -> True
        Lit{}           -> True
        App x1 x2       -> isSimpleX x1 && isAtomX x2
        Cast x1 _       -> isSimpleX x1
        _               -> False

isAtomX :: Expr a -> Bool
isAtomX xx
 = case xx of
        Var{}           -> True
        Lit{}           -> True
        _               -> False


parens' :: Doc -> Doc
parens' d = lparen <> nest 1 d <> rparen


-- | Wrap a `Doc` in parens if the predicate is true.
pprParen' :: Bool -> Doc -> Doc
pprParen' b c
 = if b then parens' c
        else c

