module Data_Lazy where

import LrcPrelude

--
--      Abstract Syntax
--
--
data P
	= C_RootProd_1 !Defs 
	deriving (Show , Eq , Ord)

data Defs
	= C_Defs2_1 !Def !Defs 
	| C_NoDefs_1 
	deriving (Show , Eq , Ord)

data Def
	= C_Arraydecl_1 !Type !Name !INT 
	| C_Declfunc_1 !Type !Name !FormPars !Stats 
	| C_Declfunc_header_1 !Type !Name !FormPars !Stats 
	| C_Declfunc_header_novar_1 !Type !Name !FormPars !Stats 
	| C_Vardecl_1 !Type !Name 
	deriving (Show , Eq , Ord)

data Type
	= C_Booltype_1 
	| C_Chartype_1 
	| C_Errortype_1 
	| C_Inttype_1 
	| C_Realtype_1 
	deriving (Show , Eq , Ord)

data Name
	= C_Ident_1 !STR 
	deriving (Show , Eq , Ord)

data FormPars
	= C_Emptyformpars_1 
	| C_Lstformpars_1 !FormPar !FormPars 
	deriving (Show , Eq , Ord)

data FormPar
	= C_Declformpar_1 !Type !Name 
	deriving (Show , Eq , Ord)

data Stats
	= C_Emptystat_1 
	| C_Lststats_1 !Stat !Stats 
	deriving (Show , Eq , Ord)

data Stat
	= C_ArrAssign_1 !ArrayUse !Exp 
	| C_Assign_1 !Name !Exp 
	| C_Funccall_1 !Name !ActPars 
	| C_If_t_e_1 !Exp !Stats !Stats 
	| C_Input_1 !Name 
	| C_LocalDecl_1 !Type !Name 
	| C_Print_1 !Exp 
	| C_While_1 !Exp !Stats 
	deriving (Show , Eq , Ord)

data ArrayUse
	= C_ArrayInd_1 !Name !Exp 
	deriving (Show , Eq , Ord)

data Exp
	= C_AddExp_1 !Exp !Exp 
	| C_AndExp_1 !Exp !Exp 
	| C_DivExp_1 !Exp !Exp 
	| C_EqExp_1 !Exp !Exp 
	| C_Factor_1 !Fac 
	| C_GTExp_1 !Exp !Exp 
	| C_LTExp_1 !Exp !Exp 
	| C_MinExp_1 !Exp 
	| C_MulExp_1 !Exp !Exp 
	| C_NotExp_1 !Exp 
	| C_OrExp_1 !Exp !Exp 
	| C_SubExp_1 !Exp !Exp 
	deriving (Show , Eq , Ord)

data Fac
	= C_ArrayConst_1 !ArrayUse 
	| C_BoolConst_1 !BOOL 
	| C_CNIdent_1 !Name 
	| C_Expr_1 !Exp 
	| C_Funcinv_1 !Name !ActPars 
	| C_IntConst_1 !INT 
	| C_RealConst_1 !REAL 
	deriving (Show , Eq , Ord)

data ActPars
	= C_Emptyactpars_1 
	| C_Lstactpars_1 !Exp !ActPars 
	deriving (Show , Eq , Ord)

data PPRoot
	= C_All_1 !PPS 
	| C_Best_1 !PPS 
	deriving (Show , Eq , Ord)

data PPS
	= C_Above_1 !PPS !PPS 
	| C_Apply_1 !PPC !PPSArgs 
	| C_Beside_1 !PPS !PPS 
	| C_Dup_1 !PPS !PPS 
	| C_Empty_1 
	| C_FillBlock_1 !INT !FillList 
	| C_Filla_1 !FillList 
	| C_Indent_1 !INT !PPS 
	| C_Join_1 !PPS 
	| C_Text_1 !STR 
	deriving (Show , Eq , Ord)

data PPC
	= C_AboveC_1 !PPC !PPC 
	| C_ApplyC_1 !PPC !PPCArgs 
	| C_BesideC_1 !PPC !PPC 
	| C_DupC_1 !PPC !PPC 
	| C_IndentC_1 !INT !PPC 
	| C_JoinC_1 !PPC 
	| C_ParC_1 
	deriving (Show , Eq , Ord)

data PPCArgs
	= C_ConsPPCArgs_1 !PPC !PPCArgs 
	| C_NilPPCArgs_1 
	deriving (Show , Eq , Ord)

data PPSArgs
	= C_ConsArgs_1 !PPS !PPSArgs 
	| C_NilArgs_1 
	deriving (Show , Eq , Ord)

data FillList
	= C_ConsFillList_1 !PPS !FillList 
	| C_NilFillList_1 
	deriving (Show , Eq , Ord)


--
--      Type of Attributes
--
--
type Code = [Instr]

type CodeParams = [Code]

data Disp
	= C_Displ_1 !PPS 
	deriving (Show , Eq , Ord)

data ENTRY
	= C_Consarray_1 !Type !INT !INT 
	| C_Consfunc_1 !Type !INT !LSTPARAM 
	| C_Consvar_1 !Type !INT 
	| C_EmptyEntry_1 
	deriving (Show , Eq , Ord)

type ERROR = [OneError]

data Format
	= C_Elem_1 !INT !INT !INT !Lst_Str 
	deriving (Show , Eq , Ord)

type Formats = [Format]

data Instr
	= C_ALabel_1 !Name 
	| C_Add_1 
	| C_And_1 
	| C_Call_1 !Name 
	| C_Cod_1 
	| C_Data_1 
	| C_Div_1 
	| C_Eq_1 
	| C_Gt_1 
	| C_Halt_1 
	| C_IIn_1 
	| C_IOut_1 
	| C_Jump_1 !Name 
	| C_Jumpf_1 !Name 
	| C_Load_1 
	| C_Lt_1 
	| C_Minus_1 
	| C_Mul_1 
	| C_Neq_1 
	| C_Not_1 
	| C_Or_1 
	| C_Pusha_1 !Name !INT 
	| C_Pushb_1 !BOOL 
	| C_Pushi_1 !INT 
	| C_Pushr_1 !REAL 
	| C_Ret_1 
	| C_Store_1 
	| C_Sub_1 
	| C_Var_1 !Name !INT !Type 
	deriving (Show , Eq , Ord)

type LSTPARAM = [OneParam]

type Lst_Str = [STR]

data OneError
	= C_E_FormParam_AD_1 !Name 
	| C_E_Fun_ND_1 !Name 
	| C_E_Loc_Name_AD_1 !Name 
	| C_E_Name_AD_1 !Name 
	| C_E_Name_ND_1 !Name 
	deriving (Show , Eq , Ord)

data OneParam
	= C_AParam_1 !Type !Name 
	deriving (Show , Eq , Ord)

data OneTypeError
	= C_E_T_ActParam_1 
	| C_E_T_BOP_1 
	| C_E_T_DT_1 !Name 
	| C_E_T_IndArrNotInt_1 
	| C_E_T_NC_1 !Type !Type 
	| C_E_T_NotArithExp_1 
	| C_E_T_NotBooleanExp_1 
	| C_E_T_if_t_e_1 
	| C_E_T_while_1 
	| C_NoTypeError_1 
	deriving (Show , Eq , Ord)

data Pair_Formats
	= C_C_Pair_Formats_1 !Formats !BOOL 
	deriving (Show , Eq , Ord)

data Pair_Lst_T_Errs
	= C_CPair_Lst_T_Errs_1 !T_Errs !T_Errs 
	deriving (Show , Eq , Ord)

data Pair_Lst_T_Fmts
	= C_CPair_Lst_T_Fmts_1 !T_Fmts !T_Fmts 
	deriving (Show , Eq , Ord)

data Pair_Lst_T_Mins
	= C_CPair_Lst_T_Mins_1 !T_Mins !T_Mins 
	deriving (Show , Eq , Ord)

data Pair_T_Formats
	= C_C_Pair_T_Formats_1 !T_Formats !BOOL 
	deriving (Show , Eq , Ord)

data Sizes
	= C_Triple_1 !INT !INT !INT 
	deriving (Show , Eq , Ord)

type TYPES = [Type]

type T_Errs = [BOOL]

type T_Fmts = [T_Formats]

data T_Formats
	= C_AFormat_1 !Formats 
	| C_TFormats_1 !Formats !Formats !BOOL !BOOL 
	deriving (Show , Eq , Ord)

data T_Frame
	= C_F_1 !INT !INT 
	deriving (Show , Eq , Ord)

type T_Mins = [Sizes]

type T_Reqs = [T_Frame]

type TypeError = [OneTypeError]

