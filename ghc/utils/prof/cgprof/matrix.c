/* ------------------------------------------------------------------------
 * $Id: matrix.c,v 1.3 2006/01/09 14:32:31 simonmar Exp $
 *									
 *	Copyright (C) 1995-2000 University of Oxford
 *									
 * Permission to use, copy, modify, and distribute this software,
 * and to incorporate it, in whole or in part, into other software,
 * is hereby granted without fee, provided that
 *   (1) the above copyright notice and this permission notice appear in
 *	 all copies of the source code, and the above copyright notice
 *	 appear in clearly visible form on all supporting documentation
 *	 and distribution media;
 *   (2) modified versions of this software be accompanied by a complete
 *	 change history describing author, date, and modifications made;
 *	 and
 *   (3) any redistribution of the software, in original or modified
 *	 form, be without fee and subject to these same conditions.
 * --------------------------------------------------------------------- */

/* Not very clever sparse representation of a matrix. However, it will do
 * for the call graph profiler. 
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "matrix.h"

Matrix newMat(int rows,int cols, int elsize, void *zero) {
  Matrix res;

  res.elsize= elsize;
  res.zero  = malloc(elsize);
  if (res.zero==NULL) {
    fprintf(stderr,"{newMat} unable to allocate storage\n");
    exit(1);
  }
  memcpy(res.zero,zero,elsize);
  res.rows  = rows;
  res.cols  = cols;
  res.mat=NULL;
  return res;
}

void freeMat(Matrix *mat) {
  Matrix_element *tmp_ptr, *ptr=mat->mat;
  free(mat->zero);

  while(ptr!=NULL) {
    free(ptr->data);
    tmp_ptr = ptr->next;
    free(ptr);
    ptr=tmp_ptr;
  }
}

void *_Mat(Matrix *mat,int x, int y,int lineno, char *filename) {
  Matrix_element *ptr= mat->mat;
  if (x<0 || x>=mat->rows || y<0 || y>=mat->cols) {
    fprintf(stderr,"Mat[%d,%d] out of bound index at line %d of \"%s\"\n",
	    x,y,lineno,filename);
    exit(1);
  }
  while(ptr) {
    if ((x==ptr->x) && (y==ptr->y)) {
      return ptr->data;
    }
    ptr=ptr->next;
  }
  /* Not in list */
  ptr = (Matrix_element*) malloc(sizeof(Matrix_element));
  if (ptr==NULL) {
    fprintf(stderr,"{_Mat} failed to allocate %zd bytes\n",
	    sizeof(Matrix_element));
    exit(1);
  }
  ptr->data = (void*) malloc(mat->elsize);
  if (ptr->data==NULL) {
    fprintf(stderr,"{_Mat} failed to allocate element of size %d bytes\n",
	    mat->elsize);
    exit(1);
  }
  ptr->x=x;
  ptr->y=y;
  memcpy(ptr->data,mat->zero,mat->elsize);
  ptr->next=mat->mat;
  mat->mat=ptr;
  return ptr->data;
}

int Mat_dense(Matrix mat,int x,int y) {
  Matrix_element *ptr= mat.mat;
  while (ptr) {
    if ((x==ptr->x) && (y==ptr->y)) return 1;
    ptr=ptr->next;
  }
  return 0;
}
