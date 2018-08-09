#!/usr/bin/env bash
#
# llvm-target generator
#
# Author: Moritz Angermann <moritz.angermann@gmail.com>
#
# This file generates the `llvm-targets` file, which contains the
# data-layout, cpu and attributes for the target.  This is done by
# querying `clang` for the data-layout, cpu and attributes based
# on a given target.
#
# To utilize it run it as
#
#  $ ./gen-data-layout.sh > llvm-targets
#
# Add missing targets to the list below to have them included in
# llvm-targets file.

# Target sets for which to generate the llvm-targets file
TARGETS=(
    # Windows x86
    "i386-unknown-windows" "i686-unknown-windows" "x86_64-unknown-windows"

    # Linux ARM
    "arm-unknown-linux-gnueabihf" "armv6-unknown-linux-gnueabihf" "armv6l-unknown-linux-gnueabihf"
    "armv7-unknown-linux-gnueabihf" "armv7a-unknown-linux-gnueabi" "armv7l-unknown-linux-gnueabihf"
    "aarch64-unknown-linux-gnu" "aarch64-unknown-linux"
    # Linux x86
    "i386-unknown-linux-gnu" "i386-unknown-linux" "x86_64-unknown-linux-gnu" "x86_64-unknown-linux"
    # Linux Android
    "armv7-unknown-linux-androideabi" "aarch64-unknown-linux-android"
    # Linux ppc64le
    "powerpc64le-unknown-linux"

    # FreeBSD amd64
    "amd64-portbld-freebsd"

    # QNX
    "arm-unknown-nto-qnx-eabi"

    # macOS
    "i386-apple-darwin" "x86_64-apple-darwin"
    # iOS
    "armv7-apple-ios arm64-apple-ios" "i386-apple-ios x86_64-apple-ios"

    # FreeBSD ARM
    "aarch64-unknown-freebsd" "armv6-unknown-freebsd-gnueabihf" "armv7-unknown-freebsd-gnueabihf"
)

# given the call to clang -c11 that clang --target -v generates,
# parse the -target-cpu <CPU> and -target-feature <feature> from
# the output.
function get_cpu_and_attr() {
    # echo $@
    while [ "$#" -gt 0 ]; do
        case "$1" in
            -target-cpu) CPU=$2; shift 2;;
            -target-feature)
                # translate clang to opt/llc target features
                case "$2" in
                    # we don't have support in GHC for proper soft-float.
                    # if we extend the `llvm-target` file to contain two
                    # additional columns for opt and llc flags, we could
                    # pass -float-abi=soft; However ghc will use float
                    # registers unconditionally on arm, and as such true
                    # soft float with the registerised llvm backend is
                    # currently not possible.
                    +soft-float-abi) shift 2;;
                    *) ATTR+=("$2"); shift 2;;
                esac
                ;;
            *) shift 1;;
        esac
    done
}

# first marker to discriminate the first line being outputted.
FST=1
# a dummy file to use for the clang invocation.
FILE=_____dummy.c
touch $FILE

for target in "${TARGETS[@]}"; do
    # find the cpu and attributes emitted by clang for the given $target
    CPU=""
    ATTR=()
    args=$(clang --target=$target -S $FILE -o /dev/null -v 2>&1 |grep $FILE)
    get_cpu_and_attr $args

    # find the data-layout from the llvm code emitted by clang.
    dl=$(clang --target=$target -S $FILE -emit-llvm -o -|grep datalayout |awk -F\  '{ print $4 }')
    # GNU and Apple/LLVM can't agree on the aarch64 target.
    # aarch64-apple-ios, is understood by autotools but not by LLVM.
    # arm64-apple-ios, is understood by LLVM, but not by autotools.
    #
    # therefore, while we query clang with arm64-apple-ios, we put
    # aarch64-apple-ios into the llvm-target list, as that is what
    # we have to configure ghc with --target with anyway.  Also we
    # want to retain the GNU naming for compatibility with libraries
    # that use autotools.
    if [ "$target" == "arm64-apple-ios" ]; then
       target="aarch64-apple-ios"
    fi
    if [ $FST -eq 1 ]; then
        echo "[(\"${target}\", ($dl, \"$CPU\", \"${ATTR[*]}\"))"
        FST=0
    else
        echo ",(\"${target}\", ($dl, \"$CPU\", \"${ATTR[*]}\"))"
    fi
done
rm $FILE
echo "]"
