FROM ubuntu:16.04

RUN apt-get update && apt-get install -y --no-install-recommends \
  autoconf \
  automake \
  bzip2 \
  ca-certificates \
  curl \
  file \
  g++ \
  git \
  make \
  openssh-client \
  patch \
  perl \
  python2.7 \
  python3 \
  software-properties-common \
  sudo \
  wget \
  xz-utils

COPY build-toolchain.sh /tmp/
RUN /tmp/build-toolchain.sh x86_64
