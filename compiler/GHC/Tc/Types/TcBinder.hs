module GHC.Tc.Types.TcBinder where

import GHC.Types.Id
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Utils.Outputable

---------------------------
-- The TcBinderStack
---------------------------

type TcBinderStack = [TcBinder]
   -- This is a stack of locally-bound ids and tyvars,
   --   innermost on top
   -- Used only in error reporting (relevantBindings in TcError),
   --   and in tidying
   -- We can't use the tcl_env type environment, because it doesn't
   --   keep track of the nesting order

type TcId = Id

data TcBinder
  = TcIdBndr
       TcId
       TopLevelFlag    -- Tells whether the binding is syntactically top-level
                       -- (The monomorphic Ids for a recursive group count
                       --  as not-top-level for this purpose.)

  | TcIdBndr_ExpType  -- Variant that allows the type to be specified as
                      -- an ExpType
       Name
       ExpType
       TopLevelFlag

  | TcTvBndr          -- e.g.   case x of P (y::a) -> blah
       Name           -- We bind the lexical name "a" to the type of y,
       TyVar          -- which might be an utterly different (perhaps
                      -- existential) tyvar

instance Outputable TcBinder where
   ppr (TcIdBndr id top_lvl)           = ppr id <> brackets (ppr top_lvl)
   ppr (TcIdBndr_ExpType id _ top_lvl) = ppr id <> brackets (ppr top_lvl)
   ppr (TcTvBndr name tv)              = ppr name <+> ppr tv

instance HasOccName TcBinder where
    occName (TcIdBndr id _)             = occName (idName id)
    occName (TcIdBndr_ExpType name _ _) = occName name
    occName (TcTvBndr name _)           = occName name