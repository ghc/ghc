#!/usr/bin/env sh

cat <<EOF
This script is for painlessly finding min and max allocations of T12545 based on
playing with -dunique-increment (see #19414) to diagnose if a metric increase is
a false positive.
A ratio of about 4.8% (48 per mille) is expected.
EOF

# https://stackoverflow.com/a/4774063/388010
TOP="$( cd -- "$(dirname "$0")/../../../../" >/dev/null 2>&1 ; pwd -P )"
GHC=${GHC:-$TOP/_validate/stage1/bin/ghc}

echo "Using GHC=$GHC. Feel free to override via env var"

function measure() {
  $GHC -fforce-recomp -v0 -dunique-increment=$1 T12545.hs +RTS -t 2>&1 | cut -f1 -d',' | grep -o -P '\d+'
}

min=999999999999
max=-999999999999
while true; do
  inc=$((1 + $RANDOM % 1000000))
  n=$(measure $inc)
  any_change=false
  if [ $n -lt $min ]; then
    min=$n
    any_change=true
    echo "New min: $min (on $inc)"
  fi
  if [ $n -gt $max ]; then
    max=$n
    any_change=true
    echo "New max: $max (on $inc)"
  fi
  if [ "$any_change" = true ]; then
    echo "New ratio: $(($max*1000/$min - 1000)) per mille"
  fi
done
