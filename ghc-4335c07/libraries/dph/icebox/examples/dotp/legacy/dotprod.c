// gcc -c -O6 dotprod.c

float cvectorDP (float *v1, float *v2, int n)
{
  int   i;
  float sum = 0;

  for (i = 0; i < n; i++)
    sum += v1[i] * v2[i];
  return sum;
}
