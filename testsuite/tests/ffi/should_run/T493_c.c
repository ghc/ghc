typedef int (*intfun_p)(int);

int dbl(int x)
{
        return x*2;
}

intfun_p getDbl(void)
{
        return dbl;
}

int apply(intfun_p f, int x)
{
        return f(x);
}
