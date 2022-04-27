#include <sys/mman.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

#include "HsBaseConfig.h"

#include "../include/syscall.h"
#include "../include/hs_uring.h"

static void *mmap_aperture(int fd, size_t len, off_t offset)
{
  return mmap(
      NULL,
      len,
      PROT_READ | PROT_WRITE,
      MAP_SHARED | MAP_POPULATE,
      fd,
      offset
  );
}

static size_t sq_len(const struct io_uring_params *params)
{
  return params->sq_off.array + params->sq_entries * sizeof(__u32);
}

static size_t sqe_len(const struct io_uring_params *params)
{
  return params->sq_entries * sizeof(struct io_uring_sqe);
}

static size_t cq_len(const struct io_uring_params *params)
{
  return params->cq_off.cqes + params->cq_entries * sizeof(struct io_uring_cqe);
}

struct hs_uring *hs_new_uring(unsigned int entries)
{
  struct hs_uring *uring = malloc(sizeof(struct hs_uring));
  if (uring == NULL) {
    fprintf(stderr, "hs_new_uring: failed allocate uring\n");
    return NULL;
  }
  memset(uring, 0, sizeof(struct hs_uring));

  struct io_uring_params *params = &uring->params;
  const int fd = io_uring_setup(entries, params);
  if (fd < 0) {
    fprintf(stderr, "hs_new_uring: failed create uring: %s\n", strerror(errno));
    goto error;
  }
  uring->uring_fd = fd;

  // SQ Ring
  uring->sq_aperture = mmap_aperture(fd, sq_len(params), IORING_OFF_SQ_RING);
  if (uring->sq_aperture == MAP_FAILED) {
    fprintf(stderr, "hs_new_uring: failed to map SQ aperture: %s\n", strerror(errno));
    goto error;
  }

  // SQE aperature
  uring->sqe_aperture = mmap_aperture(fd, sqe_len(params), IORING_OFF_SQES);
  if (uring->sqe_aperture == MAP_FAILED) {
    fprintf(stderr, "hs_new_uring: failed to map SQE aperture: %s\n", strerror(errno));
    goto error;
  }

  // CQ Ring
  uring->cq_aperture = mmap_aperture(fd, cq_len(params), IORING_OFF_CQ_RING);
  if (uring->cq_aperture == MAP_FAILED) {
    fprintf(stderr, "hs_new_uring: failed to map CQ aperture: %s\n", strerror(errno));
    goto error;
  }

  return uring;

error:
  hs_free_uring(uring);
  return NULL;
}

void hs_free_uring(struct hs_uring *uring) {
  const struct io_uring_params *params = &uring->params;

  if (uring->cq_aperture != MAP_FAILED) {
    munmap(uring->cq_aperture, cq_len(params));
  }

  if (uring->sqe_aperture != MAP_FAILED) {
    munmap(uring->sqe_aperture, sqe_len(params));
  }

  if (uring->sq_aperture != MAP_FAILED) {
    munmap(uring->sq_aperture, sq_len(params));
  }

  free(uring);
}

