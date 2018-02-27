FROM haskell:8.2
RUN echo 'Set up ghc user'
RUN adduser ghc --gecos 'GHC builds' --disabled-password

RUN echo 'Install OpenSSH client'
RUN apt-get update -qq
RUN apt-get install -qy openssh-client

USER ghc
