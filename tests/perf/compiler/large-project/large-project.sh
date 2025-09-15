#!/usr/bin/env bash

set -eu

total="$1"

for ((i = 1; i < $total; i++))
do
  # Important to write directly to variables with `-v`, otherwise the script takes a second per 1000 modules
  printf -v j "%04d" "$i"
  printf -v k "%04d" "$(($i - 1))"
  echo -e "module Mod${j} where
import Mod${k}
f_${j} :: ()
f_${j} = f_$k" > "Mod${j}.hs"
done

echo "
module Mod0000 where
f_0000 :: ()
f_0000 = ()
" > "Mod0000.hs"
