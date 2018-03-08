#!/usr/bin/env nix-shell
#! nix-shell -i bash shell.nix

# This script sets up the build environment by invoking nix-shell shell.nix
# and then runs the hadrian executable.

function rl {
    TARGET_FILE="$1"

    cd "$(dirname "$TARGET_FILE")"
    TARGET_FILE="$(basename "$TARGET_FILE")"

    # Iterate down a (possible) chain of symlinks
    while [ -L "$TARGET_FILE" ]
    do
        TARGET_FILE="$(readlink "$TARGET_FILE")"
        cd "$(dirname "$TARGET_FILE")"
        TARGET_FILE="$(basename "$TARGET_FILE")"
    done

    # Compute the canonicalized name by finding the physical path
    # for the directory we're in and appending the target file.
    PHYS_DIR="$(pwd -P)"
    RESULT="$PHYS_DIR/$TARGET_FILE"
    echo "$RESULT"
}

absoluteRoot="$(dirname "$(rl "$0")")"
echo $absoluteRoot
cd "$absoluteRoot"

hadrian         \
  --lint                         \
  --directory="$absoluteRoot/.." \
  "$@"
