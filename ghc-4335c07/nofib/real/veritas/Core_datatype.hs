{-
 * Mon Nov  5 09:54:24 GMT 1990
 *
 * Implementation of untyped terms, signatures and declarations
 *
 * Each constructors last argument (of the tuple) is a list of
 * information attributes that the parser, unparsers, tactics etc use.
 *	
 * Each terms' next to last argument is a list of alternative types that the
 * term can have to its natutal type.
 *
 -}

module Core_datatype where



type Share_map = [ Int ]

data ITrm =  Sym        Int Int                     [ ITrm ] [ Attribute ]
	   | App        ITrm ITrm                   [ ITrm ] [ Attribute ]
	   | Pair       ITrm ITrm ITrm              [ ITrm ] [ Attribute ]
	   | Binder     Binder_conn IDec ITrm       [ ITrm ] [ Attribute ]
	   | Constant   Constant_conn               [ ITrm ] [ Attribute ]
	   | Unary      Unary_conn ITrm             [ ITrm ] [ Attribute ]
	   | Binary'    Binary_conn ITrm ITrm       [ ITrm ] [ Attribute ]
	   | Cond       IDec ITrm ITrm              [ ITrm ] [ Attribute ]
	   | Const      Int Int Int                 [ ITrm ] [ Attribute ]
	   | Recurse    [ ITrm ] ITrm               [ ITrm ] [ Attribute ]
	   | Tagid      Tag [ Tag_Arg ]
	   | ITrm_Err   String
--	     deriving (Eq)
	       
data IDec =  Symbol_dec ITrm                         [ Attribute ]
	   | Axiom_dec  ITrm                         [ Attribute ]
	   | Def        ITrm ITrm                    [ Attribute ]
	   | Data       [ IDec ] [[ ITrm ]]          [ Attribute ]
	   | Decpair    IDec IDec                    [ Attribute ]
--	     deriving (Eq)
      
data ISgn =   Empty     			        [ Attribute ]
 	    | Extend     IDec ISgn                      [ Attribute ]
	    | Combine    ISgn ISgn Int [ Int ]          [ Attribute ]  
	    | Share      ISgn Int Int Int [ Int ]       [ Attribute ]
--	      deriving (Eq)

data Binder_conn = Lambda | Forall | Exists | Pi | Sigma |
		   Imp | Subtype | Delta | Choose
	     	   deriving (Eq)

data Unary_conn = Not deriving (Eq)

data Binary_conn = Or | And | Eq' | Issubtype deriving (Eq)

data Constant_conn = T | F | Bool' | Univ Int deriving (Eq)








-- types associated with tags

type Tag = ( String , [Tag_Arg_type] , [Cnv_Fn]  )


-- type indicator and resulting types for args

data Tag_Arg_type = Term_Arg | Deriv_Arg | Int_Arg deriving ( Eq )

data Tag_Arg = Tg_Trm Trm | Tg_Thm Thm | Tg_Int [Int] -- deriving ( Eq )


-- convertion function ( tags to related objects )

data Cnv_Fn = Trm_Fn ( [Tag_Arg] -> Trm ) |
	      Thm_Fn ( [Tag_Arg] -> Thm )     

-- need terms and theorems in tags ( leave here permanently? )

data Trm = TM ITrm ITrm ISgn | {- the term, its type, and signature     -}
           TM_Err String

data Thm = TH ITrm ISgn | {- the theorem, and its signature             -}
           TH_Err String






-- working types for parser

type Attribute = ( Attribute_Tag , Attribute_Value ) 


data Flagged_ITrm = Opr Operator Oprtype Int |
		      Opnd Operand  	   | 
		      Prs_Err String
--		      	deriving ( Eq )

data Operand = Itrm ITrm | Idec IDec | Isgn ISgn   -- normal operands
		 | PApp Binder_conn IDec Bool        -- special partial apps, bool indicates anonymous declaration 
		 | PairApp ITrm 
	 	 | ParIfx Binary_conn ITrm
		 | TypApp Flagged_ITrm
		 | ParColon ITrm
--		   deriving ( Eq )

data Operator = OpItrm ITrm | OpBdr Binder_conn 
		  | OpIfx Binary_conn | Spl String --deriving ( Eq )


data Attribute_Tag =   Name_Style
		     | Symbol_Style
		     | Pair_Style
		     | Let_Style
		     | Case_Style
		     | Opr_Style
		     | Fun_Style
		     | Binder_Style
		     | Constant_Style
		     | Unary_Style
		     | Binary_Style
		     | Cond_Style
		     | Recurse_Style
		     | Hyp_Style
		     | Dec_Style
		     | Def_Style
		     | Comment
		       deriving (Eq)


data Attribute_Value =   Symbol_Name Name' 
		       | Datatype_Name [ Name' ]  
		       | Named | Indexed
		       | Typed | Untyped
		       | Let
		       | Case
		       | Prefixed | Linfixed | Rinfixed | Postfixed
		       | Functional | Subscripted
		       | Prefix_Binder | Infix_Binder
		       | Recursive | Fn
		       | NonDependent
		       | Grouped | Ungrouped 
		       | Parameter | NonParameter
		       | String' String
			 deriving (Eq)

data Name' = Name String | Operator' String Int Oprtype
		deriving ( Eq )

data Oprtype = Pre | Post | BinL | BinR deriving ( Eq )

-- att_to_str (Name' s) = s

