#include<cstdio>
#include<algorithm>
using namespace std;

const int maxn=500+10;
const int mod=998244353;

long long f[maxn][maxn];
int n;
char s[maxn][5];
long long x[maxn];

int main()
{
	scanf("%d",&n);
	for (int i=1;i<=n;i++)
	{
		scanf("%s", s[i]);
		if (s[i][0]=='+') scanf("%lld",&x[i]);
	}
	long long res=0;
	for (int i=1;i<=n;i++)
	if (s[i][0]=='+')
	{
		for (int j=0;j<=n;j++)
		for (int k=0;k<=n;k++)
			f[j][k]=0;
		f[0][0]=1;
		for (int j=0;j+1<=i;j++)
		for (int k=0;k<=n;k++)
		if (f[j][k])
		{
			if (s[j+1][0]=='-')
			{
				int kk=max(k-1,0);
				f[j+1][kk] = (f[j+1][kk]+f[j][k])%mod;
			} else
			{
				int kk=k;
				if (x[j+1]<=x[i]) kk++;
				f[j+1][kk] = (f[j+1][kk]+f[j][k])%mod;
			}
			if (j+1!=i)
				f[j+1][k]=(f[j+1][k]+f[j][k])%mod;
		}
		for (int j=i;j<n;j++)
		for (int k=1;k<=n;k++)
		if (f[j][k])
		{
			if (s[j+1][0]=='-')
			{
				if (k-1)
					f[j+1][k-1] = (f[j+1][k-1]+f[j][k])%mod;
			} else
			{
				int kk=k;
				if (x[j+1]<x[i]) kk++;
				f[j+1][kk] = (f[j+1][kk]+f[j][k])%mod;
			}
			f[j+1][k]=(f[j+1][k]+f[j][k])%mod;
		}
		for (int k=1;k<=n;k++) res=(res+f[n][k]*x[i]%mod)%mod;
	}
	printf("%lld\n", res);
}

