#include<cstdio>
#include<algorithm>
using namespace std;

long long extend_gcd(long long a, long long b, long long &x, long long &y)
{
	/*
		ax + by = gcd
		(kb+a')x + by = gcd
		b(kx+y) + a'x = gcd
	*/
	
	if (b==0) {x=1;y=0;return a;}
	
	long long gcd, _x, _y;
	gcd = extend_gcd(b, a%b, _x, _y);
	x = _y;
	y = _x-a/b*x;
	return gcd;
}

int t;
long long n,a,b;

int main()
{
	for (scanf("%d",&t);t--;)
	{
		scanf("%lld%lld%lld",&n,&a,&b);
		bool flag=0;
		for (long long i=1;i<=n;i*=a)
		if ((n-i)%b==0)
		{
			flag=1;
			break;
		} else if (a==1) break;
		if (flag) printf("Yes\n");
		else printf("No\n");
	}
}
