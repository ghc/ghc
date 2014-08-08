#!/bin/sh
yum update -y
yum install -y glibc-devel ncurses-devel gmp-devel autoconf automake libtool \
 gcc make perl python ghc git docbook-utils docbook-utils-pdf docbook-style-xsl
