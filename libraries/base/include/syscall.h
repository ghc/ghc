#pragma once

#include <signal.h>
#include <linux/io_uring.h>

int io_uring_setup(unsigned entries, struct io_uring_params *p);
int io_uring_enter(
    int fd, unsigned to_submit, unsigned min_complete,
		unsigned flags, sigset_t *sig);
