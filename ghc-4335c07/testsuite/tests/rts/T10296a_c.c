typedef int (* IntFun)(int a, int b);

int apply_fun(int n, IntFun f, int a, int b) {
  int s = 0;
  int i;

  for (i = 0; i < n; i++) {
    // Each call back into Haskell using f dereferences a stable pointer
    s += f(a, b + i);
  }

  return s;
}
