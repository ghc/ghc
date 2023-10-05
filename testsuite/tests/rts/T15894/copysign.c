double
copysign (double x, double y)
{
  return x * y;
}

double
with_copysign (double x)
{
  return copysign (x, x);
}
