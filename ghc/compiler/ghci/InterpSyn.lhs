%
% (c) The University of Glasgow 2000
%
\section[InterpSyn]{Abstract syntax for interpretable trees}

\begin{code}
module InterpSyn {- Todo: ( ... ) -} where

#include "HsVersions.h"

import Id
import RdrName
import PrimOp
import Outputable

import PrelAddr -- tmp
import PrelGHC  -- tmp

-----------------------------------------------------------------------------
-- The interpretable expression type

data HValue = HValue  -- dummy type, actually a pointer to some Real Code.

data IBind con var = IBind Id (IExpr con var)

binder (IBind v e) = v
bindee (IBind v e) = e

data AltAlg  con var = AltAlg  Int{-tagNo-} [(Id,Rep)] (IExpr con var)
data AltPrim con var = AltPrim (Lit con var) (IExpr con var)

-- HACK ALERT!  A Lit may *only* be one of LitI, LitL, LitF, LitD
type Lit con var = IExpr con var

data Rep 
  = RepI 
  | RepP
  | RepF
  | RepD
  -- we're assuming that Char# is sufficiently compatible with Int# that
  -- we only need one rep for both.

  {- Not yet:
  | RepI8
  | RepI64
  -}
  deriving Eq



-- index???OffClosure needs to traverse indirection nodes.

-- You can always tell the representation of an IExpr by examining
-- its root node.
data IExpr con var
   = CaseAlgP  Id (IExpr con var) [AltAlg  con var] (Maybe (IExpr con var))
   | CaseAlgI  Id (IExpr con var) [AltAlg  con var] (Maybe (IExpr con var))

   | CasePrimP Id (IExpr con var) [AltPrim con var] (Maybe (IExpr con var))
   | CasePrimI Id (IExpr con var) [AltPrim con var] (Maybe (IExpr con var))

   -- saturated constructor apps; args are in heap order.
   -- The Addrs are the info table pointers.  Descriptors refer to the
   -- arg reps; all constructor applications return pointer rep.
   | ConApp    con
   | ConAppI   con (IExpr con var)
   | ConAppP   con (IExpr con var)
   | ConAppPP  con (IExpr con var) (IExpr con var)
   | ConAppPPP con (IExpr con var) (IExpr con var) (IExpr con var)

   | PrimOpI PrimOp [(IExpr con var)]
   | PrimOpP PrimOp [(IExpr con var)]

   | NonRecP (IBind con var) (IExpr con var)
   | RecP    [IBind con var] (IExpr con var)

   | NonRecI (IBind con var) (IExpr con var)
   | RecI    [IBind con var] (IExpr con var)

   | LitI   Int#
   | LitF   Float#
   | LitD   Double#
   | LitS   FAST_STRING

   {- not yet:
   | LitB   Int8#
   | LitL   Int64#
   -}

   | Native var	  -- pointer to a Real Closure

   | VarP   Id
   | VarI   Id
   | VarF   Id
   | VarD   Id

	-- LamXY indicates a function of reps X -> Y
	-- ie var rep = X, result rep = Y
	-- NOTE: repOf (LamXY _ _) = RepI regardless of X and Y
	--
   | LamPP  Id (IExpr con var)
   | LamPI  Id (IExpr con var)
   | LamPF  Id (IExpr con var)
   | LamPD  Id (IExpr con var)
   | LamIP  Id (IExpr con var)
   | LamII  Id (IExpr con var)
   | LamIF  Id (IExpr con var)
   | LamID  Id (IExpr con var)
   | LamFP  Id (IExpr con var)
   | LamFI  Id (IExpr con var)
   | LamFF  Id (IExpr con var)
   | LamFD  Id (IExpr con var)
   | LamDP  Id (IExpr con var)
   | LamDI  Id (IExpr con var)
   | LamDF  Id (IExpr con var)
   | LamDD  Id (IExpr con var)

	-- AppXY means apply a fn (always of Ptr rep) to 
	-- an arg of rep X giving result of Rep Y
	-- therefore: repOf (AppXY _ _) = RepY
   | AppPP  (IExpr con var) (IExpr con var)
   | AppPI  (IExpr con var) (IExpr con var)
   | AppPF  (IExpr con var) (IExpr con var)
   | AppPD  (IExpr con var) (IExpr con var)
   | AppIP  (IExpr con var) (IExpr con var)
   | AppII  (IExpr con var) (IExpr con var)
   | AppIF  (IExpr con var) (IExpr con var)
   | AppID  (IExpr con var) (IExpr con var)
   | AppFP  (IExpr con var) (IExpr con var)
   | AppFI  (IExpr con var) (IExpr con var)
   | AppFF  (IExpr con var) (IExpr con var)
   | AppFD  (IExpr con var) (IExpr con var)
   | AppDP  (IExpr con var) (IExpr con var)
   | AppDI  (IExpr con var) (IExpr con var)
   | AppDF  (IExpr con var) (IExpr con var)
   | AppDD  (IExpr con var) (IExpr con var)


showExprTag :: IExpr c v -> String
showExprTag expr
   = case expr of
        CaseAlgP  _ _ _ _ -> "CaseAlgP"
        CasePrimP _ _ _ _ -> "CasePrimP"
        CaseAlgI  _ _ _ _ -> "CaseAlgI"
        CasePrimI _ _ _ _ -> "CasePrimI"
        ConApp _          -> "ConApp"
        ConAppI _ _       -> "ConAppI"
        ConAppP _ _       -> "ConAppP"
        ConAppPP _ _ _    -> "ConAppPP"
        ConAppPPP _ _ _ _ -> "ConAppPPP"
        PrimOpI _ _       -> "PrimOpI"
        NonRecP _ _       -> "NonRecP"
        RecP _ _          -> "RecP"
        NonRecI _ _       -> "NonRecI"
        RecI _ _          -> "RecI"
        LitI _            -> "LitI"
        LitS _            -> "LitS"
        Native _          -> "Native"
        VarP _            -> "VarP"
        VarI _            -> "VarI"
        LamPP _ _         -> "LamPP"
        LamPI _ _         -> "LamPI"
        LamIP _ _         -> "LamIP"
        LamII _ _         -> "LamII"
        AppPP _ _         -> "AppPP"
        AppPI _ _         -> "AppPI"
        AppIP _ _         -> "AppIP"
        AppII _ _         -> "AppII"
        other             -> "(showExprTag:unhandled case)"

-----------------------------------------------------------------------------
-- Instantiations of the IExpr type

type UnlinkedIExpr = IExpr RdrName RdrName
type LinkedIExpr   = IExpr Addr    HValue

type UnlinkedIBind = IBind RdrName RdrName
type LinkedIBind   = IBind Addr    HValue

type UnlinkedAltAlg  = AltAlg  RdrName RdrName
type LinkedAltAlg    = AltAlg  Addr HValue

type UnlinkedAltPrim = AltPrim RdrName RdrName
type LinkedAltPrim = AltPrim Addr HValue

-----------------------------------------------------------------------------
-- Pretty printing

instance Outputable HValue where
   ppr x = text (show (A# (unsafeCoerce# x :: Addr#)))
        -- ptext SLIT("<O>")  -- unidentified lurking object

pprIBind :: (Outputable var, Outputable con) => IBind con var -> SDoc
pprIBind (IBind v e) = ppr v <+> char '=' <+> pprIExpr e

pprAltAlg (AltAlg tag vars rhs)
   = text "Tag_" <> int tag <+> hsep (map ppr vars)
     <+> text "->" <+> pprIExpr rhs

pprAltPrim (AltPrim tag rhs)
   = pprIExpr tag <+> text "->" <+> pprIExpr rhs

instance Outputable Rep where
   ppr RepI = text "I"
   ppr RepP = text "P"

instance Outputable Addr where
   ppr addr = text (show addr)

pprDefault Nothing = text "NO_DEFAULT"
pprDefault (Just e) = text "DEFAULT ->" $$ nest 2 (pprIExpr e)

pprIExpr :: (Outputable var, Outputable con) => IExpr con var -> SDoc
pprIExpr (expr:: IExpr con var)
   = case expr of
        PrimOpI op args -> doPrimOp 'I' op args
        PrimOpP op args -> doPrimOp 'P' op args

        VarI v    -> ppr v
        VarP v    -> ppr v
        LitI i#   -> int (I# i#) <> char '#'
        LitS s    -> char '"' <> ptext s <> char '"'

        LamPP v e -> doLam "PP" v e
        LamPI v e -> doLam "PI" v e
        LamIP v e -> doLam "IP" v e
        LamII v e -> doLam "II" v e

        AppPP f a -> doApp "PP" f a
        AppPI f a -> doApp "PI" f a
        AppIP f a -> doApp "IP" f a
        AppII f a -> doApp "II" f a

	Native v  -> ptext SLIT("Native") <+> ppr v

        CasePrimI b sc alts def -> doCasePrim 'I' b sc alts def
        CasePrimP b sc alts def -> doCasePrim 'P' b sc alts def

        CaseAlgI b sc alts def -> doCaseAlg 'I' b sc alts def
        CaseAlgP b sc alts def -> doCaseAlg 'P' b sc alts def

        NonRecP bind body -> doNonRec 'P' bind body
	NonRecI bind body -> doNonRec 'I' bind body

        ConApp    i          -> doConApp "" i ([] :: [IExpr con var])
        ConAppI   i a1       -> doConApp "" i [a1]
        ConAppP   i a1       -> doConApp "" i [a1]
        ConAppPP  i a1 a2    -> doConApp "" i [a1,a2]
        ConAppPPP i a1 a2 a3 -> doConApp "" i [a1,a2,a3]

        other     -> text "pprIExpr: unimplemented tag:" 
                     <+> text (showExprTag other)
     where
        doConApp repstr itbl args
           = text "Con" <> text repstr
             <+> char '[' <> hsep (map pprIExpr args) <> char ']'

        doPrimOp repchar op args
           = char repchar <> ppr op <+> char '[' <> hsep (map pprIExpr args) <> char ']'

        doNonRec repchr bind body
           = vcat [text "let" <> char repchr <+> pprIBind bind, text "in", pprIExpr body]

        doCasePrim repchr b sc alts def
           = sep [text "CasePrim" <> char repchr 
                     <+> pprIExpr sc <+> text "of" <+> ppr b <+> char '{',
                  nest 2 (vcat (map pprAltPrim alts) $$ pprDefault def),
                  char '}'
                 ]

        doCaseAlg repchr b sc alts def
           = sep [text "CaseAlg" <> char repchr 
                     <+> pprIExpr sc <+> text "of" <+> ppr b <+> char '{',
                  nest 2 (vcat (map pprAltAlg alts) $$ pprDefault def),
                  char '}'
                 ]

        doApp repstr f a
           = text "(@" <> text repstr <+> pprIExpr f <+> pprIExpr a <> char ')'
        doLam repstr v e 
           = (char '\\' <> text repstr <+> ppr v <+> text "->") $$ pprIExpr e

\end{code}
