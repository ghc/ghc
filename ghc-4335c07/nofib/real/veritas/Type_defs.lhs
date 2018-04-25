> module Type_defs where

> import Core_datatype

> import Kernel

> data MayBe a b = Ok a | Bad b deriving ( Eq )

> data Token = Rvd String | Clr String | Bdr Binder_conn |
>	       IfxBdr String | IfxOp String | Scan_Err String
>	         deriving ( Eq )
>   -- partain:
> instance Show Token


>-- data Oprtype = Pre | Post | BinL | BinR deriving ( Eq )

replace String arg to Opr by ITrm ?

following now defined in Core_datatype

>{-
> data Flagged_ITrm = Opr Operator Oprtype Int |
>		      Opnd Operand  	   | 
>		      Prs_Err String
>		      	deriving ( Eq )

> data Operand = Itrm ITrm | Idec IDec | Isgn ISgn   -- normal operands
>		 | PApp Binder_conn IDec Bool        -- special partial apps, bool indicates anonymous declaration 
>		 | PairApp ITrm 
>	 	 | ParIfx Binary_conn ITrm
>		 | TypApp Flagged_ITrm
>		   deriving ( Eq )

> data Operator = OpItrm ITrm | OpBdr Binder_conn 
>		  | OpIfx Binary_conn | Spl String deriving ( Eq )



> type Itrm_fn = Flagged_ITrm -> ITrm

> type Trm_fn = Flagged_ITrm -> Trm

> type Deriv_fn = Flagged_ITrm -> Thm

> type Tag = ( String , [Tag_Arg_type] , Itrm_fn , Trm_fn , Deriv_fn )

> data Tag_Arg_type = Term_Arg | Deriv_Arg | Int_Arg deriving ( Eq )
> -}
