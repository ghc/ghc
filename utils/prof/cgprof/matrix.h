/* ------------------------------------------------------------------------
 * $Id: matrix.h,v 1.1 2000/04/05 10:06:36 simonmar Exp $
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

#ifndef _MATRIX_H_
#define _MATRIX_H_
typedef struct _Matrix_element {
  int                     x,y;
  void                   *data;
  struct _Matrix_element *next;
} Matrix_element;

typedef struct {
  int             elsize;
  void           *zero;
  int             rows,cols;
  Matrix_element *mat;
} Matrix;


extern Matrix newMat(int,int,int,void*);
extern void   *_Mat(Matrix*,int,int,int,char*);
extern int    Mat_dense(Matrix,int,int);
extern void   freeMat(Matrix *);

#define Mat(t,m,i,j) (*((t*) _Mat(&(m),i,j,__LINE__,__FILE__)))
#endif
