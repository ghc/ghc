

module U_literal where
import UgenUtil
import Util
data U_literal = U_integer U_stringId | U_intprim U_stringId | U_floatr U_stringId | U_doubleprim U_stringId | U_floatprim U_stringId | U_charr U_hstring | U_charprim U_hstring | U_string U_hstring | U_stringprim U_hstring | U_clitlit U_stringId U_stringId | U_norepi U_stringId | U_norepr U_stringId U_stringId | U_noreps U_hstring 

rdU_literal :: _Addr -> UgnM U_literal
rdU_literal t
  = ioToUgnM (_ccall_ tliteral t) `thenUgn` \ tag@(I# _) ->
    if tag == ``integer'' then
	ioToUgnM (_ccall_ ginteger t) `thenUgn` \ x_ginteger ->
	rdU_stringId x_ginteger `thenUgn` \ y_ginteger ->
	returnUgn (U_integer y_ginteger)
    else if tag == ``intprim'' then
	ioToUgnM (_ccall_ gintprim t) `thenUgn` \ x_gintprim ->
	rdU_stringId x_gintprim `thenUgn` \ y_gintprim ->
	returnUgn (U_intprim y_gintprim)
    else if tag == ``floatr'' then
	ioToUgnM (_ccall_ gfloatr t) `thenUgn` \ x_gfloatr ->
	rdU_stringId x_gfloatr `thenUgn` \ y_gfloatr ->
	returnUgn (U_floatr y_gfloatr)
    else if tag == ``doubleprim'' then
	ioToUgnM (_ccall_ gdoubleprim t) `thenUgn` \ x_gdoubleprim ->
	rdU_stringId x_gdoubleprim `thenUgn` \ y_gdoubleprim ->
	returnUgn (U_doubleprim y_gdoubleprim)
    else if tag == ``floatprim'' then
	ioToUgnM (_ccall_ gfloatprim t) `thenUgn` \ x_gfloatprim ->
	rdU_stringId x_gfloatprim `thenUgn` \ y_gfloatprim ->
	returnUgn (U_floatprim y_gfloatprim)
    else if tag == ``charr'' then
	ioToUgnM (_ccall_ gchar t) `thenUgn` \ x_gchar ->
	rdU_hstring x_gchar `thenUgn` \ y_gchar ->
	returnUgn (U_charr y_gchar)
    else if tag == ``charprim'' then
	ioToUgnM (_ccall_ gcharprim t) `thenUgn` \ x_gcharprim ->
	rdU_hstring x_gcharprim `thenUgn` \ y_gcharprim ->
	returnUgn (U_charprim y_gcharprim)
    else if tag == ``string'' then
	ioToUgnM (_ccall_ gstring t) `thenUgn` \ x_gstring ->
	rdU_hstring x_gstring `thenUgn` \ y_gstring ->
	returnUgn (U_string y_gstring)
    else if tag == ``stringprim'' then
	ioToUgnM (_ccall_ gstringprim t) `thenUgn` \ x_gstringprim ->
	rdU_hstring x_gstringprim `thenUgn` \ y_gstringprim ->
	returnUgn (U_stringprim y_gstringprim)
    else if tag == ``clitlit'' then
	ioToUgnM (_ccall_ gclitlit t) `thenUgn` \ x_gclitlit ->
	rdU_stringId x_gclitlit `thenUgn` \ y_gclitlit ->
	ioToUgnM (_ccall_ gclitlit_kind t) `thenUgn` \ x_gclitlit_kind ->
	rdU_stringId x_gclitlit_kind `thenUgn` \ y_gclitlit_kind ->
	returnUgn (U_clitlit y_gclitlit y_gclitlit_kind)
    else if tag == ``norepi'' then
	ioToUgnM (_ccall_ gnorepi t) `thenUgn` \ x_gnorepi ->
	rdU_stringId x_gnorepi `thenUgn` \ y_gnorepi ->
	returnUgn (U_norepi y_gnorepi)
    else if tag == ``norepr'' then
	ioToUgnM (_ccall_ gnorepr_n t) `thenUgn` \ x_gnorepr_n ->
	rdU_stringId x_gnorepr_n `thenUgn` \ y_gnorepr_n ->
	ioToUgnM (_ccall_ gnorepr_d t) `thenUgn` \ x_gnorepr_d ->
	rdU_stringId x_gnorepr_d `thenUgn` \ y_gnorepr_d ->
	returnUgn (U_norepr y_gnorepr_n y_gnorepr_d)
    else if tag == ``noreps'' then
	ioToUgnM (_ccall_ gnoreps t) `thenUgn` \ x_gnoreps ->
	rdU_hstring x_gnoreps `thenUgn` \ y_gnoreps ->
	returnUgn (U_noreps y_gnoreps)
    else
	error ("rdU_literal: bad tag selection:"++show tag++"\n")
