FROM haskell:8.2
RUN echo 'Set up ghc user'
RUN adduser ghc --gecos 'GHC builds' --disabled-password

RUN echo 'Install OpenSSH client'
RUN apt-get update -qq
RUN apt-get install -qy openssh-client

RUN apt-get install -qy git make automake autoconf gcc perl python3 texinfo xz-utils lbzip2 patch
RUN apt-get install -qy curl xz-utils

RUN ln -s /home/ghc/.cabal/bin/HsColour /usr/local/bin/HsColour

USER ghc
