
for arg; do
    case "$arg" in
        --cc=*) HSC2HS_EXTRA=;;
        --)     break;;
    esac
done

$HSC2HS_DIR/$HS_PROG -t $HSC2HS_DIR/template-hsc.h $HSC2HS_EXTRA "$@" 
