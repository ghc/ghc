
for arg do
    case "$arg" in
        -c*)    HSC2HS_EXTRA=;;
        --cc=*) HSC2HS_EXTRA=;;
        --)     break;;
    esac
done

$HSC2HS_BINDIR/$HS_PROG -t $HSC2HS_DIR/template-hsc.h $HSC2HS_EXTRA "$@" 
