GOLDEN=`awk 'NF==9{if(($1!="CAF")&&($1!="IDLE")){print $1, $2, $3, $5}}' linker_unload_profiled.prof.sample`

for f in linker_unload_profiled_*.prof; do
  PROFILE=`awk 'NF==9{if(($1!="CAF")&&($1!="IDLE")){print $1, $2, $3, $5}}' $f`
  if [ "$GOLDEN" != "$PROFILE" ]; then
    echo "$f"
    echo "Expected:"
    echo "$GOLDEN"
    echo "Got:"
    echo "$PROFILE"
    exit 1
  fi
done
