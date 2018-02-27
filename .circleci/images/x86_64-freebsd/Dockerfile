FROM ubuntu:16.04

RUN apt-get update -qq
RUN apt-get install -y --no-install-recommends software-properties-common
RUN add-apt-repository -y ppa:hvr/ghc
RUN apt-get update -qq
RUN apt-get install -y --no-install-recommends \
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
  xz-utils \
  ghc-8.0.2 \
  cabal-install-1.24 \
  alex \
  happy \
  ncurses-dev \
  gcc \
  texinfo \
  lbzip2

COPY build-toolchain.sh /tmp/
RUN /tmp/build-toolchain.sh x86_64
RUN ln -s $HOME/.cabal/bin/HsColour /usr/local/bin/HsColour
