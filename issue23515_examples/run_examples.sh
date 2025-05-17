#!/bin/bash

# Directory where the script is located
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Path to GHC (stage 1 build)
GHC_PATH="$DIR/../_build/stage1/bin/ghc"

# Make sure the script is executable
chmod +x "$0"

# Run each example file with GHC in interactive mode
for example in "$DIR"/Example*.hs; do
  echo "-----------------------------------------------"
  echo "Running: $example"
  echo "-----------------------------------------------"
  "$GHC_PATH" --interactive "$example"
  echo ""
done