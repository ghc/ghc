#!/bin/sh

# When reproducing the Haskell files we want to to be sure that the files that
# we used to generate them earlier are exactly the same as the ones we are
# downloading. To ensure that verification of the checksum is necessary.

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
. "$SCRIPT_DIR/unicode_version"

# When downloading fresh new version comment this out
VERIFY_CHECKSUM=y

# Filename:checksum
FILES="\
  ucd/DerivedCoreProperties.txt:d367290bc0867e6b484c68370530bdd1a08b6b32404601b8c7accaf83e05628d \
  ucd/UnicodeData.txt:806e9aed65037197f1ec85e12be6e8cd870fc5608b4de0fffd990f689f376a73"

# Download the files

# Download $file from https://www.unicode.org/Public/$VERSION/$file
# and verify the $checksum if $VERIFY_CHECKSUM is enabled
# $1 = file:checksum
download_file() {
    __pair="$1"
    __file="$(echo "$__pair" | cut -f1 -d':')"
    __local_file="$SCRIPT_DIR/$__file"
    __checksum="$(echo "$__pair" | cut -f2 -d':')"

    if test ! -e "$__local_file"
    then
        wget -P "$(dirname "$__local_file")" "https://www.unicode.org/Public/$VERSION/$__file"
    fi
    if test -n "$VERIFY_CHECKSUM"
    then
        new_checksum=$(sha256sum "$__local_file" | cut -f1 -d' ')
        if test "$__checksum" != "$new_checksum"
        then
            echo "sha256sum of the downloaded __file $__file "
            echo "   [$new_checksum] does not match the expected checksum [$__checksum]"
            exit 1
        else
            echo "$__file checksum ok"
        fi
    fi
}

# Extract $file from $FILES download it using download_file
download_files() {
    for pair in $FILES
    do
        download_file "$pair"
    done
}

GHC_MODULE_PATH=$(realpath "$SCRIPT_DIR/../../")

# Generate the Haskell files.
run_generator() {
    # Compile and run ucd2haskell
    cabal run exe:ucd2haskell -- \
          --input "$SCRIPT_DIR/ucd/" \
          --output "$GHC_MODULE_PATH/" \
          --core-prop Uppercase \
          --core-prop Lowercase
        # [NOTE] disabled generator
        #   --core-prop Alphabetic 
        #   --core-prop White_Space \
        #   --core-prop ID_Start \
        #   --core-prop ID_Continue \
        #   --core-prop XID_Start \
        #   --core-prop XID_Continue \
        #   --core-prop Pattern_Syntax \
        #   --core-prop Pattern_White_Space
}

# Print help text
print_help() {
    echo "Usage: ucd.sh <command>"
    echo
    echo "Available commands:"
    echo "  download: downloads the text files required"
    echo "  generate: generate the haskell files from the downloaded text files"
    echo
    echo "Example:"
    echo "$ ./ucd.sh download && ./ucd.sh generate"
}

# Main program

# Export the version so it can be used by the executable
export UNICODE_VERSION="$VERSION"

# Parse command line
case $1 in
    -h|--help) print_help;;
    download) download_files;;
    generate) run_generator;;
    *) echo "Unknown argument"; print_help;;
esac
