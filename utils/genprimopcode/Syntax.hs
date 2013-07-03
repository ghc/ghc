module Syntax where

import Data.List

------------------------------------------------------------------
-- Abstract syntax -----------------------------------------------
------------------------------------------------------------------

-- info for all primops; the totality of the info in primops.txt(.pp)
data Info
   = Info [Option] [Entry]   -- defaults, primops
     deriving Show

-- info for one primop
data Entry
    = PrimOpSpec { cons  :: String,      -- PrimOp name
                   name  :: String,      -- name in prog text
                   ty    :: Ty,          -- type
                   cat   :: Category,    -- category
                   desc  :: String,      -- description
                   opts  :: [Option] }   -- default overrides
    | PseudoOpSpec { name  :: String,      -- name in prog text
                     ty    :: Ty,          -- type
                     desc  :: String,      -- description
                     opts  :: [Option] }   -- default overrides
    | PrimTypeSpec { ty    :: Ty,      -- name in prog text
                     desc  :: String,      -- description
                     opts  :: [Option] }   -- default overrides
    | Section { title :: String,         -- section title
                desc  :: String }        -- description
    deriving Show

is_primop :: Entry -> Bool
is_primop (PrimOpSpec _ _ _ _ _ _) = True
is_primop _ = False

-- a binding of property to value
data Option
   = OptionFalse  String          -- name = False
   | OptionTrue   String          -- name = True
   | OptionString String String   -- name = { ... unparsed stuff ... }
   | OptionInteger String Int     -- name = <int>
   | OptionFixity (Maybe Fixity)  -- fixity = infix{,l,r} <int> | Nothing
     deriving Show

-- categorises primops
data Category
   = Dyadic | Monadic | Compare | GenPrimOp
     deriving Show

-- types
data Ty
   = TyF    Ty Ty
   | TyApp  TyCon [Ty]
   | TyVar  TyVar
   | TyUTup [Ty]   -- unboxed tuples; just a TyCon really, 
                   -- but convenient like this
   deriving (Eq,Show)

type TyVar = String
type TyCon = String

-- Follow definitions of Fixity and FixityDirection in GHC

data Fixity = Fixity Int FixityDirection
  deriving (Eq, Show)

data FixityDirection = InfixN | InfixL | InfixR
  deriving (Eq, Show)

------------------------------------------------------------------
-- Sanity checking -----------------------------------------------
------------------------------------------------------------------

{- Do some simple sanity checks:
    * all the default field names are unique
    * for each PrimOpSpec, all override field names are unique
    * for each PrimOpSpec, all overriden field names   
          have a corresponding default value
    * that primop types correspond in certain ways to the 
      Category: eg if Comparison, the type must be of the form
         T -> T -> Bool.
   Dies with "error" if there's a problem, else returns ().
-}
myseqAll :: [()] -> a -> a
myseqAll (():ys) x = myseqAll ys x
myseqAll []      x = x

sanityTop :: Info -> ()
sanityTop (Info defs entries)
   = let opt_names = map get_attrib_name defs
         primops = filter is_primop entries
     in  
     if   length opt_names /= length (nub opt_names)
     then error ("non-unique default attribute names: " ++ show opt_names ++ "\n")
     else myseqAll (map (sanityPrimOp opt_names) primops) ()

sanityPrimOp :: [String] -> Entry -> ()
sanityPrimOp def_names p
   = let p_names = map get_attrib_name (opts p)
         p_names_ok
            = length p_names == length (nub p_names)
              && all (`elem` def_names) p_names
         ty_ok = sane_ty (cat p) (ty p)
     in
         if   not p_names_ok
         then error ("attribute names are non-unique or have no default in\n" ++
                     "info for primop " ++ cons p ++ "\n")
         else
         if   not ty_ok
         then error ("type of primop " ++ cons p ++ " doesn't make sense w.r.t" ++
                     " category " ++ show (cat p) ++ "\n")
         else ()

sane_ty :: Category -> Ty -> Bool
sane_ty Compare (TyF t1 (TyF t2 td)) 
   | t1 == t2 && td == TyApp "Int#" []  = True
sane_ty Monadic (TyF t1 td) 
   | t1 == td  = True
sane_ty Dyadic (TyF t1 (TyF t2 td))
   | t1 == td && t2 == td  = True
sane_ty GenPrimOp _
   = True
sane_ty _ _
   = False

get_attrib_name :: Option -> String
get_attrib_name (OptionFalse nm) = nm
get_attrib_name (OptionTrue nm)  = nm
get_attrib_name (OptionString nm _) = nm
get_attrib_name (OptionInteger nm _) = nm
get_attrib_name (OptionFixity _) = "fixity"

lookup_attrib :: String -> [Option] -> Maybe Option
lookup_attrib _ [] = Nothing
lookup_attrib nm (a:as) 
    = if get_attrib_name a == nm then Just a else lookup_attrib nm as

