#!/bin/bash

# Directory where the script is located
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Path to GHC (stage 1 build)
GHC_PATH="$DIR/../_build/stage1/bin/ghc"

# Make sure the script is executable
chmod +x "$0"

# Check if an example number was specified
if [ -n "$1" ]; then
  # Run a specific example
  example="$DIR/Example$1.hs"
  if [ -f "$example" ]; then
    echo "-----------------------------------------------"
    echo "Running: $example"
    echo "-----------------------------------------------"
    "$GHC_PATH" --interactive "$example"
    echo ""
  else
    echo "Example file $example not found"
    exit 1
  fi
else
  # Run all examples
  for example in "$DIR"/Example*.hs; do
    echo "-----------------------------------------------"
    echo "Running: $example"
    echo "-----------------------------------------------"
    "$GHC_PATH" --interactive "$example"
    echo ""
  done
fi