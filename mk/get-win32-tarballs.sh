#!/usr/bin/env sh

tarball_dir='ghc-tarballs'
missing_files=0

fail() {
    echo >&2
    echo "$1" >&2
    exit 1
}

download_file() {
    local file_url="$1"
    local file_md5="$2"
    local dest_file="$3"
    local description="$4"
    local extra_curl_opts="$5"
    local dest_dir="$(dirname $dest_file)"

    if ! test -f "${dest_file}"
    then
        local curl_cmd="curl -L ${file_url} -o ${dest_file} --create-dirs -# ${extra_curl_opts}"
        if test "$download" = "0"
        then
            echo "ERROR: Missing ${description}" >&2
            echo "${file_url}"
            missing_files=1
            return
        else
            echo "Downloading ${description} to ${dest_dir}..."
            $curl_cmd || {
                rm -f "${dest_file}"
                fail "ERROR: Download failed."
            }
        fi
    fi

    echo "${file_md5} *${dest_file}" | md5sum --quiet -c - ||
        fail "ERROR: ${description} appears to be corrupted, please delete it and try again."
}

download_mingw() {
    local mingw_url="$1"
    local file_md5sum_x86="$2"
    local file_md5sum_x64="$3"

    if test "$mingw_arch" = "i686"
    then
        local file_md5sum="${file_md5sum_x86}"
    else
        local file_md5sum="${file_md5sum_x64}"
    fi

    local mingw_toolchain="$(basename $mingw_url)"
    local mingw_w64="${tarball_dir}/${tarball_dest_dir}/${mingw_toolchain}"

    download_file "${mingw_url}" "${file_md5sum}" "${mingw_w64}" "${mingw_toolchain}"

    # Mark the tree as needing updates by deleting the folder
    if test -d inplace/mingw && test inplace/mingw -ot "$mingw_w64" ; then
        echo "In-tree MinGW-w64 tree requires updates..."
        rm -rf inplace/mingw
    fi
}

download_tarballs() {
    local mingw_base_url="https://downloads.haskell.org/~ghc/mingw"
    local package_prefix="mingw-w64"
    local format_url="${mingw_base_url}/${mingw_arch}/${package_prefix}-${mingw_arch}"

    download_mingw "${format_url}-crt-git-5.0.0.4531.49c7046-1-any.pkg.tar.xz"           "dd39323140c0c1b3e065e9edb1a66779" "ac22cedd38229bcd57f5999e4734054f"
    download_mingw "${format_url}-winpthreads-git-5.0.0.4538.78dca70-1-any.pkg.tar.xz"   "0b14fe27790e94db454fbb3564e79a73" "65cf07b6f42a1a62d1844e08190cab0d"
    download_mingw "${format_url}-headers-git-5.0.0.4531.49c7046-1-any.pkg.tar.xz"       "6ee9e3c2f9d3e507f60ee33d19417dc2" "f49a19cdea93998c33ac90ceb9570350"
    download_mingw "${format_url}-libwinpthread-git-5.0.0.4538.78dca70-1-any.pkg.tar.xz" "fbb2114aa7fbb5507e21d8a2ea265cfd" "31ed10e2d8891f6251d968f81bfdd274"
    download_mingw "${format_url}-zlib-1.2.8-8-any.pkg.tar.xz"                           "7f519cb6defa27a90c5353160cf088d4" "6a2f4a70ccb24acca70a01da331699a6"
    download_mingw "${format_url}-isl-0.14.1-2-any.pkg.tar.xz"                           "4cd20fe75ed9ef03e260d529042cb742" "dc0e0a7fd23a8193cccb0bf8d7267685"
    download_mingw "${format_url}-mpc-1.0.3-2-any.pkg.tar.xz"                            "719e76fa7a54a8676d2e60af3bb13c45" "df1a7d4050568d83c265ae78c32ef30b"
    download_mingw "${format_url}-mpfr-3.1.3.p0-2-any.pkg.tar.xz"                        "e9cbd2402ac1afe6e86c102223b90dcb" "6e3b9ec27edab394aa41536839afdafe"
    download_mingw "${format_url}-gmp-6.0.0-3-any.pkg.tar.xz"                            "c02f9759cd0140a6d8ea69ef5a88e167" "2970d4d8b176f8f36ae2d39269b25cce"
    download_mingw "${format_url}-gcc-libs-5.2.0-3-any.pkg.tar.xz"                       "a9bd2e65cb350cc8f8a6deb6d3b346a8" "9c2ed24989e14fdf0c548a5215374660"
    download_mingw "${format_url}-binutils-2.25.1-1-any.pkg.tar.xz"                      "997e9c2166fb851916cd8ac1bc9c6180" "7cb9f5f50a7103da41f7ec7547c09707"
    download_mingw "${format_url}-libiconv-1.14-5-any.pkg.tar.xz"                        "2c99a163689ba8257627bb07274b3f86" "37418c6be92ef20be17cdc9fe844af35"
    download_mingw "${format_url}-gcc-5.2.0-3-any.pkg.tar.xz"                            "efe6d6afc18aab89dc01e7ddcd2523a6" "0b697ce61112ba6e5a3c4d565957ea4e"

    download_file "https://github.com/ghc/ghc-tarballs/blob/master/perl/ghc-perl-1.tar.gz?raw=true" "b21d1681b61cf7a024e854096285b02e" "ghc-tarballs/perl/ghc-perl-1.tar.gz" "Windows Perl binary distributions" "--insecure"

    if ! test "$missing_files" = "0"
    then
        exit 2
    fi
}

download_i386() {
    mingw_arch="i686"
    tarball_dest_dir="mingw-w64/x86"
    download_tarballs
}

download_x86_64() {
    mingw_arch="x86_64"
    tarball_dest_dir="mingw-w64/x86_64"
    download_tarballs
}

usage() {
    echo "$0 - Download GHC mingw toolchain tarballs"
    echo
    echo "Usage: $0 <action> <arch>"
    echo
    echo "Where <action> is one of,"
    echo "    download     download the necessary tarballs for the given architecture"
    echo "    verify       verify the existance and correctness of the necessary tarballs"
    echo "and <arch> is one of i386, x86_64, or all"
}

case $1 in
    download)
        download=1
        ;;
    verify)
        download=0
        ;;
    *)
        usage
        exit 1
        ;;
esac

case $2 in
    i386)
        download_i386
        ;;
    x86_64)
        download_x86_64
        ;;
    all)
        download_i386
        download_x86_64
        ;;
    *)
        usage
        exit 1
        ;;
esac
