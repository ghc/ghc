
tflag="--template=$HSC2HS_DIR/template-hsc.h"
for arg do
    case "$arg" in
        -c*)    HSC2HS_EXTRA=;;
        --cc=*) HSC2HS_EXTRA=;;
	-t*)	tflag=;;
	--template=*) tflag=;;
        --)     break;;
    esac
done

$HSC2HS_BINDIR/$HS_PROG $tflag $HSC2HS_EXTRA "$@" 
