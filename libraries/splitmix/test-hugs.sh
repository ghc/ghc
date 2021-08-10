#!/bin/sh

set -e

CABAL=${CABAL:-cabal}
HC=${HC:-ghc}

# Install cpphs if it is not in path
command -v cpphs || ${CABAL} v2-install --ignore-project --with-compiler "$HC" cpphs

# Regenerate splitmix-hugs
sh make-hugs.sh
find splitmix-hugs

die() {
    echo "TEST FAILED"
    exit 1
}

dotest() {
  echo "TEST $2"
  echo "$2" | hugs -98 -P:splitmix-hugs -p'> ' "$1" | tee hugs.output
  grep "$3" hugs.output || die
}

# Simple tests
dotest System.Random.SplitMix   "nextInteger (-100) 73786976294838206464 (mkSMGen 42)" "(10417309031967932979,SMGen 18209985878117922550 13679457532755275413)"
dotest System.Random.SplitMix32 "nextInteger (-100) 73786976294838206464 (mkSMGen 42)" "(63481308251723623759,SMGen 2735861347 1604540297)"

dotest System.Random.SplitMix   "nextWord64    (mkSMGen 42)" "(1275548033995301424,SMGen 4530528345362647137 13679457532755275413)"
dotest System.Random.SplitMix   "nextWord32    (mkSMGen 42)" "(3292324400,SMGen 4530528345362647137 13679457532755275413)"
dotest System.Random.SplitMix   "nextTwoWord32 (mkSMGen 42)" "(296986669,3292324400,SMGen 4530528345362647137 13679457532755275413)"
dotest System.Random.SplitMix   "nextInt       (mkSMGen 42)" "(296986669,SMGen 4530528345362647137 13679457532755275413)"
dotest System.Random.SplitMix   "nextDouble    (mkSMGen 42)" "(0.069147597478366,SMGen 4530528345362647137 13679457532755275413)"
dotest System.Random.SplitMix   "splitSMGen    (mkSMGen 42)" "(SMGen 18209985878117922550 13679457532755275413,SMGen 1275548033995301424 10514482549683702313)"

dotest System.Random.SplitMix   "bitmaskWithRejection64 9 (mkSMGen 43)" "(5,SMGen 15756003094639068574 13432527470776545161)"
dotest System.Random.SplitMix   "bitmaskWithRejection64' 9 (mkSMGen 44)" "(1,SMGen 3943641360161606062 18105923034897077331)"

dotest System.Random.SplitMix32   "nextWord64    (mkSMGen 42)" "(5568638952296597105,SMGen 3351673966 1604540297)"
dotest System.Random.SplitMix32   "nextWord32    (mkSMGen 42)" "(1296549791,SMGen 1747133669 1604540297)"
dotest System.Random.SplitMix32   "nextTwoWord32 (mkSMGen 42)" "(1296549791,2315961969,SMGen 3351673966 1604540297)"
dotest System.Random.SplitMix32   "nextInt       (mkSMGen 42)" "(1296549791,SMGen 1747133669 1604540297)"
dotest System.Random.SplitMix32   "nextDouble    (mkSMGen 42)" "(0.301876522493369,SMGen 3351673966 1604540297)"
dotest System.Random.SplitMix32   "splitSMGen    (mkSMGen 42)" "(SMGen 3351673966 1604540297,SMGen 1296549791 306293903)"

dotest System.Random.SplitMix32   "bitmaskWithRejection64 9 (mkSMGen 43)" "(1,SMGen 261660480 2569677503)"
dotest System.Random.SplitMix32   "bitmaskWithRejection64' 9 (mkSMGen 44)" "(8,SMGen 3882168239 2439575023)"
