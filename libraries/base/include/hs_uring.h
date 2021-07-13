#pragma once

#include "HsBaseConfig.h"
#if defined(HAVE_IO_URING)

#include <linux/io_uring.h>

#endif

struct hs_uring {
  void *sqe_aperture;
  void *sq_aperture;
  void *cq_aperture;
  int uring_fd;
  struct io_uring_params params;
};

struct hs_uring *hs_new_uring(unsigned int entries);
void hs_free_uring(struct hs_uring *uring);
