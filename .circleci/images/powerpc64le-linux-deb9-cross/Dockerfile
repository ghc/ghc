FROM ghcci/x86_64-linux-deb9

ENV LANG C.UTF-8
RUN apt-get install qemu-user && \
    dpkg --add-architecture ppc64el && \
    apt-get install crossbuild-essential-ppc64el libc6:ppc64el ncurses:ppc64el

