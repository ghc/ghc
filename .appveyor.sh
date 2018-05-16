# Configure the environment
MSYSTEM=MINGW64
THREADS=9
SKIP_PERF_TESTS=YES
BUILD_FLAVOUR=
source /etc/profile || true # a terrible, terrible workaround for msys2 brokenness

# Don't set -e until after /etc/profile is sourced
set -ex
cd $APPVEYOR_BUILD_FOLDER

case "$1" in
    "prepare")
        # Prepare the tree
        git config remote.origin.url git://github.com/ghc/ghc.git
        git config --global url."git://github.com/ghc/packages-".insteadOf git://github.com/ghc/packages/
        git submodule init
        git submodule --quiet update --recursive
        ;;
    "build")
        # Build the compiler
        ./boot
        cat <<EOF >> mk/build.mk
        BuildFlavour=$BUILD_FLAVOUR
        ifneq "\$(BuildFlavour)" ""
        include mk/flavours/\$(BuildFlavour).mk
        endif
EOF
        ./configure --enable-tarballs-autodownload
        make -j$THREADS
        ;;

    "test")
        make binary-dist
        curl https://ghc-artifacts.s3.amazonaws.com/tools/ghc-artifact-collector-x86_64-windows --output ghc-artifact-collector
        ./ghc-artifact-collector *.tar.xz
        make test THREADS=$THREADS
        ;;

    *)
        echo "$0: unknown mode $1"
        exit 1
        ;;
esac
