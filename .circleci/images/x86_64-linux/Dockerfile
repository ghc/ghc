FROM haskell:8.2

# Make sure we have proper openssh before checkout: CircleCI git
# does not check the repository out properly without it and also
# takes 20 times longer than it should be.
RUN apt-get update -qq
RUN apt-get install -qy git make automake autoconf gcc perl python3 texinfo xz-utils lbzip2 patch openssh-client sudo -qq curl

# Create a normal user.
RUN adduser ghc --gecos "GHC builds" --disabled-password
RUN echo "ghc ALL = NOPASSWD : ALL" > /etc/sudoers.d/ghc
USER ghc

WORKDIR /home/ghc/

CMD ["bash"]
