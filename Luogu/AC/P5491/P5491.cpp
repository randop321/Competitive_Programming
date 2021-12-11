#include<cstdio>
#include<algorithm>
using namespace std;

long long n, p, r, b, t, s, q;

long long pow(long long a, long long b, long long p)
{
    long long res=1;
    while (b)
    {
        if (b&1) res=res*a%p;
        a=a*a%p;
        b/=2;
    }
    return res;
}

long long process(long long r, long long n, long long t, long long m, long long zq, long long p)
{
    if (t==1) return r;
    if (pow(t, 1LL<<(m-1), p) == 1) 
        return process(r, n, t, m-1, zq, p);
    long long b = pow(zq, 1LL << (s-1-m), p);
    return process(r*b%p, n, t*b%p*b%p, m-1, zq, p);
}

int main()
{
    int ts;
    for (scanf("%d",&ts);ts--;)
    {
        scanf("%lld%lld",&n,&p);
        n%=p;
        if (n==0)
        {
            printf("0\n");
            continue;
        }
        if (pow(n,(p-1)/2, p) == p-1)
        {
            printf("Hola!\n");
            continue;
        }
        
        q = p-1;
        s = 0;
        while (!(q&1))
        {
            q /= 2;
            s++;
        }
        long long z;
        for (;;)
        {
            z = (long long)rand()*rand() % (p-1) + 1;
            if (pow(z, (p-1)/2, p) == p-1)
                break;
        }
        
        
        r = pow(n, (q+1)/2, p);
        t = pow(n, q, p);
        b = pow(z, q, p);
        r = process(r, n, t, s-1, b, p);
        long long r2 = p-r;
        //printf("%lld %lld\n", r, r2);        
        if (r>r2) swap(r,r2);
        printf("%lld %lld\n", r, r2);
    }
}
