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

    if test "$sigs" = "1"
    then
        echo "Downloading ${description} (signature) to ${dest_dir}..."
        local curl_cmd="curl -L ${file_url}.sig -o ${dest_file}.sig --create-dirs -# ${extra_curl_opts}"
        $curl_cmd || {
                rm -f "${dest_file}.sig"
                fail "ERROR: Download failed."
            }
    fi

    if test "$verify" = "1"
    then
        echo "${file_md5} *${dest_file}" | md5sum --quiet -c - ||
            fail "ERROR: ${description} appears to be corrupted, please delete it and try again."
    fi
}

download_mingw() {
    if test "$mingw_arch" = "sources"
    then
        local mingw_url=`echo "$1" | sed -e 's/-any\.pkg\.tar\.xz/\.src\.tar\.gz/' \
                                         -e 's/-sources-/-/' \
                                         -e 's/-libwinpthread-git-/-winpthreads-git-/' `
    else
        local mingw_url="$1"
    fi
    local file_md5sum_x86="$2"
    local file_md5sum_x64="$3"

    if ! test "$mingw_arch" = "sources"
    then
        if test "$mingw_arch" = "i686"
        then
            local file_md5sum="${file_md5sum_x86}"
        else
            local file_md5sum="${file_md5sum_x64}"
        fi
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
    #local mingw_base_url="http://repo.msys2.org/mingw"
    local mingw_base_url="https://downloads.haskell.org/~ghc/mingw"
    local package_prefix="mingw-w64"
    local format_url="${mingw_base_url}/${mingw_arch}/${package_prefix}-${mingw_arch}"

    download_mingw "${format_url}-crt-git-5.0.0.4795.e3d96cb1-1-any.pkg.tar.xz"          "534bb4756482f3271308576cdadfe5dc" "3780a25a6f20eef9b143f47f4b615e39"
    download_mingw "${format_url}-winpthreads-git-5.0.0.4741.2c8939a-1-any.pkg.tar.xz"   "155845f8c897f0c70adee83cfa9ec30c" "ba417ad9fb7cd3ee56e713b2b070adb9"
    download_mingw "${format_url}-headers-git-5.0.0.4747.0f8f626-1-any.pkg.tar.xz"       "b724d1aaae73c329022ad22374481817" "e8065928b81c9b379286515913eccd68"
    download_mingw "${format_url}-libwinpthread-git-5.0.0.4741.2c8939a-1-any.pkg.tar.xz" "65b18b67eef3c3d5e5707577dfa8f831" "c280f60a4b80ed6722ce4d9b4f6c550e"
    download_mingw "${format_url}-zlib-1.2.8-9-any.pkg.tar.xz"                           "87c65e9b2930436a75dfd7d459ae98cb" "60c3a388478f411b7a0908441ebeb537"
    download_mingw "${format_url}-isl-0.17.1-1-any.pkg.tar.xz"                           "9fce16db004f00e967eb15efe0cdf86b" "39c8b3b8e56b3b0bdef86cf32f1e09ba"
    download_mingw "${format_url}-mpc-1.0.3-2-any.pkg.tar.xz"                            "719e76fa7a54a8676d2e60af3bb13c45" "df1a7d4050568d83c265ae78c32ef30b"
    download_mingw "${format_url}-mpfr-3.1.4.p3-4-any.pkg.tar.xz"                        "6fdad8f6a522c779932ca4e54e4d7977" "de629f78e908274086a272196c14d37c"
    download_mingw "${format_url}-gmp-6.1.1-1-any.pkg.tar.xz"                            "e8cc05fc566ddc6c16266da9aec2ddd3" "0faa10641da9266ef4cb39a8f6a4fa19"
    download_mingw "${format_url}-gcc-libs-6.2.0-2-any.pkg.tar.xz"                       "e6f74da9dcb856cfe9e1da0ed45732e0" "f523d52a6ad940e1cda2bf3065927bd4"
    download_mingw "${format_url}-binutils-2.27-2-any.pkg.tar.xz"                        "d263d1362dee0c24df80b461eb2ec489" "b1f21340136b75f1660d6ad36bd0768a"
    download_mingw "${format_url}-libidn-1.32-3-any.pkg.tar.xz"                          "9ecd264a3da0f0f6af8b392c1b183a7b" "6f68259e17b68bbf19efc7b4fb5c1968"
    download_mingw "${format_url}-gcc-6.2.0-2-any.pkg.tar.xz"                            "095dc33fb7a1cab5dab982aa57713a96" "303bf95f8e6ac5bc068b2ab95749b8f3"

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

download_sources() {
    mingw_arch="sources"
    tarball_dest_dir="mingw-w64/sources"
    download_tarballs
}

usage() {
    echo "$0 - Download GHC mingw toolchain tarballs"
    echo
    echo "Usage: $0 <action> <arch>"
    echo
    echo "Where <action> is one of,"
    echo "    download     download the necessary tarballs for the given architecture"
    echo "    fetch        download the necessary tarballs for the given architecture but doesn't verify their md5."d
    echo "    verify       verify the existence and correctness of the necessary tarballs"
    echo "and <arch> is one of i386, x86_64,all or mirror (which includes sources)"
}

case $1 in
    download)
        download=1
        verify=1
        sigs=0
        ;;
    fetch)
        download=1
        verify=0
        ;;
    verify)
        download=0
        verify=1
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
    mirror)
        sigs=1
        download_i386
        download_x86_64
        verify=0
        download_sources
        ;;
    *)
        usage
        exit 1
        ;;
esac
