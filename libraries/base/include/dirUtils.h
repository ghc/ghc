/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-
 *
 * Directory Runtime Support - prototypes.
 */
#ifndef __DIRUTILS_H__
#define __DIRUTILS_H__

#include "HsCore.h"

#include <limits.h>

extern HsInt prel_mkdir(HsAddr pathName, HsInt mode);
extern HsInt prel_lstat(HsAddr fname, HsAddr st);

extern HsInt prel_s_ISDIR(mode_t m);
extern HsInt prel_s_ISREG(mode_t m);

extern HsInt prel_sz_stat();
extern HsInt prel_path_max();
extern mode_t prel_R_OK();
extern mode_t prel_W_OK();
extern mode_t prel_X_OK();

extern mode_t prel_S_IRUSR();
extern mode_t prel_S_IWUSR();
extern mode_t prel_S_IXUSR();

extern time_t prel_st_mtime(struct stat* st);
extern mode_t prel_st_mode(struct stat* st);

extern HsAddr prel_d_name(struct dirent* d);

extern HsInt prel_end_of_dir();

#endif /* __DIRUTILS_H__ */
