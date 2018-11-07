int mult(int a[], int b[], int N)
{
  int sum = 0;
  for(int i=0; i<N; i++){
     sum += a[i] + b[i];
  }
  return sum;
}
