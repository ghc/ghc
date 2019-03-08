#!/bin/sh
yum update -y
yum install -y glibc-devel ncurses-devel gmp-devel autoconf automake libtool \
 gcc make python ghc git
