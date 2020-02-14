/*
 *  Copyright(c), 2002 The GHC Team.
 */

#ifdef aix_HOST_OS
#define _LINUX_SOURCE_COMPAT 
// Required to get CMSG_SPACE/CMSG_LEN macros.  See #265.
// Alternative is to #define COMPAT_43 and use the 
// HAVE_STRUCT_MSGHDR_MSG_ACCRIGHTS code instead, but that means
// fiddling with the configure script too.
#endif

#include "HsNet.h"
#include <string.h>

#if HAVE_STRUCT_MSGHDR_MSG_CONTROL || HAVE_STRUCT_MSGHDR_MSG_ACCRIGHTS /* until end */

/* 
 *  Support for transmitting file descriptors.
 *
 *  
 */
 

/*
 * sendmsg() and recvmsg() wrappers for transmitting
 * ancillary socket data.
 *
 * Doesn't provide the full generality of either, specifically:
 *
 *  - no support for scattered read/writes.
 *  - only possible to send one ancillary chunk of data at a time.
 */

int
sendFd(int sock,
       int outfd)
{
  struct msghdr msg = {0};
  struct iovec iov[1];
  char  buf[2];
#if HAVE_STRUCT_MSGHDR_MSG_ACCRIGHTS
  msg.msg_accrights = (void*)&outfd;
  msg.msg_accrightslen = sizeof(int);
#else
  struct cmsghdr *cmsg;
  char ancBuffer[CMSG_SPACE(sizeof(int))];
  char* dPtr;
  
  msg.msg_control = ancBuffer;
  msg.msg_controllen = sizeof(ancBuffer);

  cmsg = CMSG_FIRSTHDR(&msg);
  cmsg->cmsg_level = SOL_SOCKET;
  cmsg->cmsg_type = SCM_RIGHTS;
  cmsg->cmsg_len = CMSG_LEN(sizeof(int));
  dPtr = (char*)CMSG_DATA(cmsg);
  
  *(int*)dPtr = outfd;
  msg.msg_controllen = cmsg->cmsg_len;
#endif

  buf[0] = 0; buf[1] = '\0';
  iov[0].iov_base = buf;
  iov[0].iov_len  = 2;

  msg.msg_iov = iov;
  msg.msg_iovlen = 1;
  
  return sendmsg(sock,&msg,0);
}

int
recvFd(int sock)
{
  struct msghdr msg = {0};
  char  duffBuf[10];
  int rc;
  int len = sizeof(int);
  struct iovec iov[1];
#if HAVE_STRUCT_MSGHDR_MSG_CONTROL
  struct cmsghdr *cmsg = NULL;
  struct cmsghdr *cptr;
#else
  int* fdBuffer;
#endif
  int fd;

  iov[0].iov_base = duffBuf;
  iov[0].iov_len  = sizeof(duffBuf);
  msg.msg_iov = iov;
  msg.msg_iovlen = 1;

#if HAVE_STRUCT_MSGHDR_MSG_CONTROL
  cmsg = (struct cmsghdr*)malloc(CMSG_SPACE(len));
  if (cmsg==NULL) {
    return -1;
  }
  
  msg.msg_control = (void *)cmsg;
  msg.msg_controllen = CMSG_LEN(len);
#else
  fdBuffer = (int*)malloc(len);
  if (fdBuffer) {
    msg.msg_accrights    = (void *)fdBuffer;
  } else {
    return -1;
  }
  msg.msg_accrightslen = len;
#endif

  if ((rc = recvmsg(sock,&msg,0)) < 0) {
#if HAVE_STRUCT_MSGHDR_MSG_CONTROL
    free(cmsg);
#else
    free(fdBuffer);
#endif
    return rc;
  }
  
#if HAVE_STRUCT_MSGHDR_MSG_CONTROL
  cptr = (struct cmsghdr*)CMSG_FIRSTHDR(&msg);
  fd = *(int*)CMSG_DATA(cptr);
  free(cmsg);
#else
  fd = *(int*)fdBuffer;
  free(fdBuffer);
#endif
  return fd;
}

#endif
