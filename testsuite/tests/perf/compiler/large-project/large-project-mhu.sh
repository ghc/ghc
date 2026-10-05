#!/usr/bin/env bash
# Usage: ./large-project-mhu.sh <num-units> <modules-per-unit>

set -eu

units="$1"
per_unit="$2"
branch="3"
unit_args=""

for ((u = 0; u < units; u++)); do
  printf -v un "unit%04d" "$u"
  pu=$(( (u - 1) / branch ))
  mkdir -p "$un"
  printf -- '-this-unit-id %s\n-i\n-i%s\n-outputdir _build/%s\n' "$un" "$un" "$un" > "$un.args"
  if (( u > 0 )); then printf -- '-package-id unit%04d\n' "$pu" >> "$un.args"; fi
  unit_args+=" -unit @$un.args"

  for ((m = 0; m < per_unit; m++)); do
    i=$(( u * per_unit + m ))
    printf -v j "%04d" "$i"
    if (( m > 0 )); then
      parent=$(( u * per_unit + (m - 1) / branch ))
    elif (( u > 0 )); then
      parent=$(( pu * per_unit + per_unit - 1 ))
    else
      parent=-1
    fi
    if (( parent < 0 )); then
      imp="" body="()"
    else
      printf -v k "%04d" "$parent"
      imp="import Mod$k"$'\n' body="f_$k"
    fi
    printf 'module Mod%s where\n%sf_%s :: ()\nf_%s = %s\n' "$j" "$imp" "$j" "$j" "$body" > "$un/Mod$j.hs"
    echo "Mod$j" >> "$un.args"
  done
done
