

module U_treeHACK where
import UgenUtil
import Util

import U_binding
import U_coresyn	( U_coresyn )	-- interface only
import U_hpragma	( U_hpragma )	-- interface only
import U_list
import U_literal
import U_ttype

type U_infixTree = (ProtoName, U_tree, U_tree)

rdU_infixTree :: _Addr -> UgnM U_infixTree
rdU_infixTree pt
  = ioToUgnM (_casm_ ``%r = gident(*Rginfun((struct Sap *)%0));'' pt) `thenUgn` \ op_t ->
    ioToUgnM (_casm_ ``%r = (*Rginarg1((struct Sap *)%0));'' pt) `thenUgn` \ arg1_t ->
    ioToUgnM (_casm_ ``%r = (*Rginarg2((struct Sap *)%0));'' pt) `thenUgn` \ arg2_t ->

    rdU_unkId op_t		`thenUgn` \ op	 ->
    rdU_tree  arg1_t		`thenUgn` \ arg1 ->
    rdU_tree  arg2_t		`thenUgn` \ arg2 ->
    returnUgn (op, arg1, arg2)

data U_tree = U_hmodule U_stringId U_list U_list U_binding U_long | U_ident U_unkId | U_lit U_literal | U_tuple U_list | U_ap U_tree U_tree | U_lambda U_list U_tree U_long | U_let U_binding U_tree | U_casee U_tree U_list | U_ife U_tree U_tree U_tree | U_par U_tree | U_as U_unkId U_tree | U_lazyp U_tree | U_plusp U_tree U_literal | U_wildp | U_restr U_tree U_ttype | U_comprh U_tree U_list | U_qual U_tree U_tree | U_guard U_tree | U_def U_tree | U_tinfixop U_infixTree | U_lsection U_tree U_unkId | U_rsection U_unkId U_tree | U_eenum U_tree U_list U_list | U_llist U_list | U_ccall U_stringId U_stringId U_list | U_scc U_hstring U_tree | U_negate U_tree 

rdU_tree :: _Addr -> UgnM U_tree
rdU_tree t
  = ioToUgnM (_ccall_ ttree t) `thenUgn` \ tag@(I# _) ->
    if tag == ``hmodule'' then
	ioToUgnM (_ccall_ ghname t) `thenUgn` \ x_ghname ->
	rdU_stringId x_ghname `thenUgn` \ y_ghname ->
	ioToUgnM (_ccall_ ghimplist t) `thenUgn` \ x_ghimplist ->
	rdU_list x_ghimplist `thenUgn` \ y_ghimplist ->
	ioToUgnM (_ccall_ ghexplist t) `thenUgn` \ x_ghexplist ->
	rdU_list x_ghexplist `thenUgn` \ y_ghexplist ->
	ioToUgnM (_ccall_ ghmodlist t) `thenUgn` \ x_ghmodlist ->
	rdU_binding x_ghmodlist `thenUgn` \ y_ghmodlist ->
	ioToUgnM (_ccall_ ghmodline t) `thenUgn` \ x_ghmodline ->
	rdU_long x_ghmodline `thenUgn` \ y_ghmodline ->
	returnUgn (U_hmodule y_ghname y_ghimplist y_ghexplist y_ghmodlist y_ghmodline)
    else if tag == ``ident'' then
	ioToUgnM (_ccall_ gident t) `thenUgn` \ x_gident ->
	rdU_unkId x_gident `thenUgn` \ y_gident ->
	returnUgn (U_ident y_gident)
    else if tag == ``lit'' then
	ioToUgnM (_ccall_ glit t) `thenUgn` \ x_glit ->
	rdU_literal x_glit `thenUgn` \ y_glit ->
	returnUgn (U_lit y_glit)
    else if tag == ``tuple'' then
	ioToUgnM (_ccall_ gtuplelist t) `thenUgn` \ x_gtuplelist ->
	rdU_list x_gtuplelist `thenUgn` \ y_gtuplelist ->
	returnUgn (U_tuple y_gtuplelist)
    else if tag == ``ap'' then
	ioToUgnM (_ccall_ gfun t) `thenUgn` \ x_gfun ->
	rdU_tree x_gfun `thenUgn` \ y_gfun ->
	ioToUgnM (_ccall_ garg t) `thenUgn` \ x_garg ->
	rdU_tree x_garg `thenUgn` \ y_garg ->
	returnUgn (U_ap y_gfun y_garg)
    else if tag == ``lambda'' then
	ioToUgnM (_ccall_ glampats t) `thenUgn` \ x_glampats ->
	rdU_list x_glampats `thenUgn` \ y_glampats ->
	ioToUgnM (_ccall_ glamexpr t) `thenUgn` \ x_glamexpr ->
	rdU_tree x_glamexpr `thenUgn` \ y_glamexpr ->
	ioToUgnM (_ccall_ glamline t) `thenUgn` \ x_glamline ->
	rdU_long x_glamline `thenUgn` \ y_glamline ->
	returnUgn (U_lambda y_glampats y_glamexpr y_glamline)
    else if tag == ``let'' then
	ioToUgnM (_ccall_ gletvdeflist t) `thenUgn` \ x_gletvdeflist ->
	rdU_binding x_gletvdeflist `thenUgn` \ y_gletvdeflist ->
	ioToUgnM (_ccall_ gletvexpr t) `thenUgn` \ x_gletvexpr ->
	rdU_tree x_gletvexpr `thenUgn` \ y_gletvexpr ->
	returnUgn (U_let y_gletvdeflist y_gletvexpr)
    else if tag == ``casee'' then
	ioToUgnM (_ccall_ gcaseexpr t) `thenUgn` \ x_gcaseexpr ->
	rdU_tree x_gcaseexpr `thenUgn` \ y_gcaseexpr ->
	ioToUgnM (_ccall_ gcasebody t) `thenUgn` \ x_gcasebody ->
	rdU_list x_gcasebody `thenUgn` \ y_gcasebody ->
	returnUgn (U_casee y_gcaseexpr y_gcasebody)
    else if tag == ``ife'' then
	ioToUgnM (_ccall_ gifpred t) `thenUgn` \ x_gifpred ->
	rdU_tree x_gifpred `thenUgn` \ y_gifpred ->
	ioToUgnM (_ccall_ gifthen t) `thenUgn` \ x_gifthen ->
	rdU_tree x_gifthen `thenUgn` \ y_gifthen ->
	ioToUgnM (_ccall_ gifelse t) `thenUgn` \ x_gifelse ->
	rdU_tree x_gifelse `thenUgn` \ y_gifelse ->
	returnUgn (U_ife y_gifpred y_gifthen y_gifelse)
    else if tag == ``par'' then
	ioToUgnM (_ccall_ gpare t) `thenUgn` \ x_gpare ->
	rdU_tree x_gpare `thenUgn` \ y_gpare ->
	returnUgn (U_par y_gpare)
    else if tag == ``as'' then
	ioToUgnM (_ccall_ gasid t) `thenUgn` \ x_gasid ->
	rdU_unkId x_gasid `thenUgn` \ y_gasid ->
	ioToUgnM (_ccall_ gase t) `thenUgn` \ x_gase ->
	rdU_tree x_gase `thenUgn` \ y_gase ->
	returnUgn (U_as y_gasid y_gase)
    else if tag == ``lazyp'' then
	ioToUgnM (_ccall_ glazyp t) `thenUgn` \ x_glazyp ->
	rdU_tree x_glazyp `thenUgn` \ y_glazyp ->
	returnUgn (U_lazyp y_glazyp)
    else if tag == ``plusp'' then
	ioToUgnM (_ccall_ gplusp t) `thenUgn` \ x_gplusp ->
	rdU_tree x_gplusp `thenUgn` \ y_gplusp ->
	ioToUgnM (_ccall_ gplusi t) `thenUgn` \ x_gplusi ->
	rdU_literal x_gplusi `thenUgn` \ y_gplusi ->
	returnUgn (U_plusp y_gplusp y_gplusi)
    else if tag == ``wildp'' then
	returnUgn (U_wildp )
    else if tag == ``restr'' then
	ioToUgnM (_ccall_ grestre t) `thenUgn` \ x_grestre ->
	rdU_tree x_grestre `thenUgn` \ y_grestre ->
	ioToUgnM (_ccall_ grestrt t) `thenUgn` \ x_grestrt ->
	rdU_ttype x_grestrt `thenUgn` \ y_grestrt ->
	returnUgn (U_restr y_grestre y_grestrt)
    else if tag == ``comprh'' then
	ioToUgnM (_ccall_ gcexp t) `thenUgn` \ x_gcexp ->
	rdU_tree x_gcexp `thenUgn` \ y_gcexp ->
	ioToUgnM (_ccall_ gcquals t) `thenUgn` \ x_gcquals ->
	rdU_list x_gcquals `thenUgn` \ y_gcquals ->
	returnUgn (U_comprh y_gcexp y_gcquals)
    else if tag == ``qual'' then
	ioToUgnM (_ccall_ gqpat t) `thenUgn` \ x_gqpat ->
	rdU_tree x_gqpat `thenUgn` \ y_gqpat ->
	ioToUgnM (_ccall_ gqexp t) `thenUgn` \ x_gqexp ->
	rdU_tree x_gqexp `thenUgn` \ y_gqexp ->
	returnUgn (U_qual y_gqpat y_gqexp)
    else if tag == ``guard'' then
	ioToUgnM (_ccall_ ggexp t) `thenUgn` \ x_ggexp ->
	rdU_tree x_ggexp `thenUgn` \ y_ggexp ->
	returnUgn (U_guard y_ggexp)
    else if tag == ``def'' then
	ioToUgnM (_ccall_ ggdef t) `thenUgn` \ x_ggdef ->
	rdU_tree x_ggdef `thenUgn` \ y_ggdef ->
	returnUgn (U_def y_ggdef)
    else if tag == ``tinfixop'' then
--	ioToUgnM (_ccall_ gdummy t) `thenUgn` \ x_gdummy ->
	rdU_infixTree t {-THIS IS THE HACK-} `thenUgn` \ y_gdummy ->
	returnUgn (U_tinfixop y_gdummy)
    else if tag == ``lsection'' then
	ioToUgnM (_ccall_ glsexp t) `thenUgn` \ x_glsexp ->
	rdU_tree x_glsexp `thenUgn` \ y_glsexp ->
	ioToUgnM (_ccall_ glsop t) `thenUgn` \ x_glsop ->
	rdU_unkId x_glsop `thenUgn` \ y_glsop ->
	returnUgn (U_lsection y_glsexp y_glsop)
    else if tag == ``rsection'' then
	ioToUgnM (_ccall_ grsop t) `thenUgn` \ x_grsop ->
	rdU_unkId x_grsop `thenUgn` \ y_grsop ->
	ioToUgnM (_ccall_ grsexp t) `thenUgn` \ x_grsexp ->
	rdU_tree x_grsexp `thenUgn` \ y_grsexp ->
	returnUgn (U_rsection y_grsop y_grsexp)
    else if tag == ``eenum'' then
	ioToUgnM (_ccall_ gefrom t) `thenUgn` \ x_gefrom ->
	rdU_tree x_gefrom `thenUgn` \ y_gefrom ->
	ioToUgnM (_ccall_ gestep t) `thenUgn` \ x_gestep ->
	rdU_list x_gestep `thenUgn` \ y_gestep ->
	ioToUgnM (_ccall_ geto t) `thenUgn` \ x_geto ->
	rdU_list x_geto `thenUgn` \ y_geto ->
	returnUgn (U_eenum y_gefrom y_gestep y_geto)
    else if tag == ``llist'' then
	ioToUgnM (_ccall_ gllist t) `thenUgn` \ x_gllist ->
	rdU_list x_gllist `thenUgn` \ y_gllist ->
	returnUgn (U_llist y_gllist)
    else if tag == ``ccall'' then
	ioToUgnM (_ccall_ gccid t) `thenUgn` \ x_gccid ->
	rdU_stringId x_gccid `thenUgn` \ y_gccid ->
	ioToUgnM (_ccall_ gccinfo t) `thenUgn` \ x_gccinfo ->
	rdU_stringId x_gccinfo `thenUgn` \ y_gccinfo ->
	ioToUgnM (_ccall_ gccargs t) `thenUgn` \ x_gccargs ->
	rdU_list x_gccargs `thenUgn` \ y_gccargs ->
	returnUgn (U_ccall y_gccid y_gccinfo y_gccargs)
    else if tag == ``scc'' then
	ioToUgnM (_ccall_ gsccid t) `thenUgn` \ x_gsccid ->
	rdU_hstring x_gsccid `thenUgn` \ y_gsccid ->
	ioToUgnM (_ccall_ gsccexp t) `thenUgn` \ x_gsccexp ->
	rdU_tree x_gsccexp `thenUgn` \ y_gsccexp ->
	returnUgn (U_scc y_gsccid y_gsccexp)
    else if tag == ``negate'' then
	ioToUgnM (_ccall_ gnexp t) `thenUgn` \ x_gnexp ->
	rdU_tree x_gnexp `thenUgn` \ y_gnexp ->
	returnUgn (U_negate y_gnexp)
    else
	error ("rdU_tree: bad tag selection:"++show tag++"\n")
