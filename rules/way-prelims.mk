
define way-prelims # $1 = way
ifeq "$1" "v"
$1__way  =
$1_way_  =
else
$1__way  = _$1
$1_way_  = $1_
endif
$1_osuf   = $$($1_way_)o
$1_hisuf  = $$($1_way_)hi
$1_hcsuf  = $$($1_way_)hc
endef
