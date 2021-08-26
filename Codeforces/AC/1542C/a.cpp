#include<cstdio>
#include<algorithm>
using namespace std;

long long gcd(long long a, long long b)
{
	if (b==0) return a;
	return gcd(b,a%b);
}

const long long mod=1000000007;
int t;
long long n;

int main()
{
	for (scanf("%d",&t);t--;)
	{
		scanf("%lld",&n);
		
		long long res=0;
		

		
		for (long long i=2,k=1;k<=n;i++)
		{
			if (k%i)
			{
				long long kk = i/gcd(i,k);
				long long tmp=n/k-n/k/kk;
				res = (res + tmp%mod*i)%mod;
			}
			k = k/gcd(k,i)*i;
		}
		printf("%lld\n", res);
	}
}
