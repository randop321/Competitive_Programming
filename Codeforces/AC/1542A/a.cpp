#include<cstdio>

int t,n,a,b;

int main()
{
	for (scanf("%d",&t);t--;)
	{
		scanf("%d",&n);
		a=b=0;
		for (int i=0,x;i<n*2;i++)
		{
			scanf("%d",&x);
			if (x&1) a++;
			else b++;
		}
		if (a==b) printf("Yes\n");
		else printf("No\n");
	}
}	
