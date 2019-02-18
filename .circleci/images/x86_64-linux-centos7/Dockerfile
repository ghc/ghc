FROM centos:7

ENV LANG C.UTF-8

# Core build utilities
RUN yum -y install coreutils binutils which git make \
    automake autoconf gcc perl python3 texinfo xz lbzip2 \
    patch openssh-clients sudo zlib-devel sqlite \
    ncurses-compat-libs gmp-devel ncurses-devel gcc-c++ findutils \
    curl wget jq

# Python 3
RUN yum install -y https://centos7.iuscommunity.org/ius-release.rpm; \
    yum install -y python34u python34u-libs

# Documentation tools
RUN yum -y install python-sphinx \
    texlive texlive-latex texlive-xetex \
    texlive-collection-latex texlive-collection-latexrecommended \
    texlive-xetex-def texlive-collection-xetex \
    python-sphinx-latex dejavu-sans-fonts dejavu-serif-fonts \
    dejavu-sans-mono-fonts

# This is in the PATH when I ssh into the CircleCI machine but somehow
# sphinx-build isn't found during configure unless we explicitly 
# add it here as well; perhaps PATH is being overridden by CircleCI's
# infrastructure?
ENV PATH /usr/libexec/python3-sphinx:$PATH

# systemd isn't running so remove it from nsswitch.conf
# Failing to do this will result in testsuite failures due to
# non-functional user lookup (#15230).
RUN sed -i -e 's/systemd//g' /etc/nsswitch.conf

# Install GHC and cabal
RUN cd /tmp && curl https://downloads.haskell.org/~ghc/8.4.4/ghc-8.4.4-x86_64-centos70-linux.tar.xz | tar -Jx; \
    cd /tmp/ghc-8.4.4 && ./configure --prefix=/opt/ghc/8.4.4; \
    cd /tmp/ghc-8.4.4 && make install; \
    rm -rf /tmp/ghc-8.4.4
ENV PATH /opt/ghc/8.4.4/bin:$PATH

# Get Cabal
RUN cd /tmp && \
    curl https://downloads.haskell.org/cabal/cabal-install-2.4.1.0/cabal-install-2.4.1.0-x86_64-unknown-linux.tar.xz | tar -Jx && \
    mv cabal /usr/local/bin/cabal

# Create a normal user.
RUN adduser ghc --comment "GHC builds"
RUN echo "ghc ALL = NOPASSWD : ALL" > /etc/sudoers.d/ghc
USER ghc
WORKDIR /home/ghc/

# Build Haskell tools
RUN cabal v2-update && \
    cabal v2-install hscolour happy alex
ENV PATH /home/ghc/.cabal/bin:$PATH

CMD ["bash"]
