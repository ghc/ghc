--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module Haddock.HaddockDB (ppDocBook) where

{-
import HaddockTypes
import HaddockUtil
import HsSyn2

import Text.PrettyPrint
-}

-----------------------------------------------------------------------------
-- Printing the results in DocBook format

ppDocBook = error "not working"
{-
ppDocBook :: FilePath -> [(Module, Interface)] -> String
ppDocBook odir mods = render (ppIfaces mods)

ppIfaces mods
  =  text "<!DOCTYPE BOOK PUBLIC \"-//OASIS//DTD DocBook V3.1//EN\" ["
  $$ text "]>"
  $$ text "<book>"
  $$ text "<bookinfo>"
  $$ text "<author><othername>HaskellDoc version 0.0</othername></author>"
  $$ text "</bookinfo>"
  $$ text "<article>"
  $$ vcat (map do_mod mods)
  $$ text "</article></book>"
  where
     do_mod (Module mod, iface)
        =  text "<sect1 id=\"sec-" <> text mod <> text "\">"
        $$ text "<title><literal>" 
	   <> text mod
	   <> text "</literal></title>"
	$$ text "<indexterm><primary><literal>"
	   <> text mod
	   <> text "</literal></primary></indexterm>"
	$$ text "<variablelist>"
	$$ vcat (map (do_export mod) (eltsFM (iface_decls iface)))
	$$ text "</variablelist>"
	$$ text "</sect1>"
 
     do_export mod decl | (nm:_) <- declBinders decl
	=  text "<varlistentry id=" <> ppLinkId mod nm <> char '>'
	$$ text "<term><literal>" 
		<> do_decl decl
		<> text "</literal></term>"
	$$ text "<listitem>"
	$$ text "<para>"
	$$ text "</para>"
	$$ text "</listitem>"
	$$ text "</varlistentry>"
     do_export _ _ = empty

     do_decl (HsTypeSig _ [nm] ty _) 
	=  ppHsName nm <> text " :: " <> ppHsType ty
     do_decl (HsTypeDecl _ nm args ty _)
	=  hsep ([text "type", ppHsName nm ]
		 ++ map ppHsName args 
		 ++ [equals, ppHsType ty])
     do_decl (HsNewTypeDecl loc ctx nm args con drv _)
	= hsep ([text "data", ppHsName nm] -- data, not newtype
		++ map ppHsName args
		) <+> equals <+> ppHsConstr con -- ToDo: derivings
     do_decl (HsDataDecl loc ctx nm args cons drv _)
	= hsep ([text "data", {-ToDo: context-}ppHsName nm]
	        ++ map ppHsName args)
            <+> vcat (zipWith (<+>) (equals : repeat (char '|'))
                                    (map ppHsConstr cons))
     do_decl (HsClassDecl loc ty fds decl _)
	= hsep [text "class", ppHsType ty]
     do_decl decl
	= empty

ppHsConstr :: HsConDecl -> Doc
ppHsConstr (HsRecDecl pos name tvs ctxt fieldList maybe_doc) =
	 ppHsName name
	 <> (braces . hsep . punctuate comma . map ppField $ fieldList)
ppHsConstr (HsConDecl pos name tvs ctxt typeList maybe_doc) = 
	 hsep (ppHsName name : map ppHsBangType typeList)

ppField (HsFieldDecl ns ty doc)
   = hsep (punctuate comma (map ppHsName ns) ++
	 	[text "::", ppHsBangType ty])

ppHsBangType :: HsBangType -> Doc
ppHsBangType (HsBangedTy ty) = char '!' <> ppHsType ty
ppHsBangType (HsUnBangedTy ty) = ppHsType ty

ppHsContext :: HsContext -> Doc
ppHsContext []      = empty
ppHsContext context = parenList (map (\ (a,b) -> ppHsQName a <+> 
					 hsep (map ppHsAType b)) context)

ppHsType :: HsType -> Doc
ppHsType (HsForAllType Nothing context htype) =
     hsep [ ppHsContext context, text "=>", ppHsType htype]
ppHsType (HsForAllType (Just tvs) [] htype) =
     hsep (text "forall" : map ppHsName tvs ++ text "." : [ppHsType htype])
ppHsType (HsForAllType (Just tvs) context htype) =
     hsep (text "forall" : map ppHsName tvs ++ text "." : 
	   ppHsContext context : text "=>" : [ppHsType htype])
ppHsType (HsTyFun a b) = fsep [ppHsBType a, text "-&gt;", ppHsType b]
ppHsType (HsTyIP n t)  = fsep [(char '?' <> ppHsName n), text "::", ppHsType t]
ppHsType t = ppHsBType t

ppHsBType (HsTyApp (HsTyCon (Qual (Module "Prelude") (HsTyClsName (HsSpecial "[]")))) b )
  = brackets $ ppHsType b
ppHsBType (HsTyApp a b) = fsep [ppHsBType a, ppHsAType b]
ppHsBType t = ppHsAType t

ppHsAType :: HsType -> Doc
ppHsAType (HsTyTuple True l)  = parenList . map ppHsType $ l
ppHsAType (HsTyTuple False l) = ubxParenList . map ppHsType $ l
-- special case
ppHsAType (HsTyApp (HsTyCon (Qual (Module "Prelude") (HsTyClsName (HsSpecial "[]")))) b )
  = brackets $ ppHsType b
ppHsAType (HsTyVar name) = ppHsName name
ppHsAType (HsTyCon name) = ppHsQName name
ppHsAType t = parens $ ppHsType t

ppHsQName :: HsQName -> Doc
ppHsQName (UnQual str)			= ppHsName str
ppHsQName n@(Qual (Module mod) str)
	 | n == unit_con_name		= ppHsName str
	 | isSpecial str 		= ppHsName str
	 | otherwise 
		=  text "<link linkend=" <> ppLinkId mod str <> char '>'
		<> ppHsName str
		<> text "</link>"

isSpecial (HsTyClsName id) | HsSpecial _ <- id = True
isSpecial (HsVarName id) | HsSpecial _ <- id = True
isSpecial _ = False

ppHsName :: HsName -> Doc
ppHsName (HsTyClsName id) = ppHsIdentifier id
ppHsName (HsVarName id) = ppHsIdentifier id

ppHsIdentifier :: HsIdentifier -> Doc
ppHsIdentifier (HsIdent str)	= text str
ppHsIdentifier (HsSymbol str) = text str
ppHsIdentifier (HsSpecial str) = text str

ppLinkId :: String -> HsName -> Doc
ppLinkId mod str
  = hcat [char '\"', text mod, char '.', ppHsName str, char '\"']

-- -----------------------------------------------------------------------------
-- * Misc

parenList :: [Doc] -> Doc
parenList = parens . fsep . punctuate comma

ubxParenList :: [Doc] -> Doc
ubxParenList = ubxparens . fsep . punctuate comma

ubxparens p = text "(#" <> p <> text "#)"
-}
