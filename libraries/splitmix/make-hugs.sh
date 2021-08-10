#!/bin/sh

set -e

TOPDIR=$(dirname "$0")
TARGETDIR=$TOPDIR/splitmix-hugs

while getopts 't:' opt
do
  case "$opt" in
    t) TARGETDIR=$OPTARG ;;
	*) echo "Unknown flag $opt"; exit 1 ;;
  esac
done

# Check tool availability
cpphs --version

# For each of the source files
find "$TOPDIR/src" "$TOPDIR/src-compat" -name '*.hs' | while read -r src; do
   tgt="$TARGETDIR/$(echo "$src" | sed "s/^$TOPDIR\/src"'\(-compat\|\)//')"

   echo "Processing $src -> $tgt"

   mkdir -p "$(dirname "$tgt")"
   cpphs --noline -D__HUGS__=1 "$src" > "$tgt"
done

echo "A Hugs-compatible version of splitmix is now"
echo "available in the splitmix-hugs directory."
echo "Load it with hugs -98."
