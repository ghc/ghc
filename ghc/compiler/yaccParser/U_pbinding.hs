

module U_pbinding where
import UgenUtil
import Util

import U_binding
import U_coresyn	( U_coresyn )	-- interface only
import U_hpragma	( U_hpragma )	-- interface only
import U_list
import U_literal	( U_literal )	-- ditto
import U_treeHACK
import U_ttype		( U_ttype )	-- ditto
data U_pbinding = U_pgrhs U_tree U_list U_binding U_stringId U_long 

rdU_pbinding :: _Addr -> UgnM U_pbinding
rdU_pbinding t
  = ioToUgnM (_ccall_ tpbinding t) `thenUgn` \ tag@(I# _) ->
    if tag == ``pgrhs'' then
	ioToUgnM (_ccall_ ggpat t) `thenUgn` \ x_ggpat ->
	rdU_tree x_ggpat `thenUgn` \ y_ggpat ->
	ioToUgnM (_ccall_ ggdexprs t) `thenUgn` \ x_ggdexprs ->
	rdU_list x_ggdexprs `thenUgn` \ y_ggdexprs ->
	ioToUgnM (_ccall_ ggbind t) `thenUgn` \ x_ggbind ->
	rdU_binding x_ggbind `thenUgn` \ y_ggbind ->
	ioToUgnM (_ccall_ ggfuncname t) `thenUgn` \ x_ggfuncname ->
	rdU_stringId x_ggfuncname `thenUgn` \ y_ggfuncname ->
	ioToUgnM (_ccall_ ggline t) `thenUgn` \ x_ggline ->
	rdU_long x_ggline `thenUgn` \ y_ggline ->
	returnUgn (U_pgrhs y_ggpat y_ggdexprs y_ggbind y_ggfuncname y_ggline)
    else
	error ("rdU_pbinding: bad tag selection:"++show tag++"\n")
