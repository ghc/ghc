/*variable bandwidth matrix ll decomposition , and system solution */

#include <math.h>

vblldecomp(n,diag,vbm)

int n,diag[];
float  vbm[];

{
  int i,j,k,adr,j0,k0;
  float  t,x;

  for (i=1; i<=n; i=i+1) {
    
    j0 = i - diag[i] + diag[i-1] + 1; /* column of first non zero elemnt */

    for (j=j0; j<i; j=j+1) {
      
      t = 0.0; 
      k0 = j - diag[j] + diag[j-1] + 1; /* fst.non.zero.col of jth row */
      if (j0>k0) k0 = j0;
      for (k=k0; k<=j-1; k=k+1) {
        x = vbm[diag[i]+k-i] * vbm[diag[j]+k-j];
        t = t + x;
      }
      adr = diag[i]+j-i;
      vbm[adr] = (vbm[adr]-t)/vbm[diag[j]];
  
    }

    t = 0.0; 
    for (k=j0; k<=(i-1); k=k+1) {
      x = vbm[diag[i]+k-i];
      t = t + x*x;
    }
    adr = diag[i];
    vbm[adr] = sqrt ( vbm[adr] - t );

  }
  

}
  

vbllsolution(n,diag,mvb,b)

int n, diag[];
float  mvb[], b[];

{
  int i,j,j0;
  float  t;

  for (i=1; i<=n; i=i+1) {
    t = 0.0;
    j0 = i - diag[i] + diag[i-1] + 1;
    for (j=j0; j<i; j=j+1) {
      t = t + b[j] * mvb[diag[i]+j-i];
    }
    b[i] = (b[i]-t) / mvb[diag[i]];
  }


  for (i=n; i>=1; i=i-1) {
    b[i] = b[i] / mvb[diag[i]];
    j0 = i - diag[i] + diag[i-1] + 1;
    for (j=j0; j<i; j=j+1) {
      b[j] = b[j] - b[i]*mvb[diag[i]+j-i];
    }
  }


}
