#include<cstdio>
#include<ctime>
#include<algorithm>
using namespace std;

const int maxn=1000;
const int maxm=3000;

int main()
{
    srand( (unsigned) time(NULL) );


    printf("%d\n",maxn);
    for (int i=0;i<maxn;i++)
        printf("%c",(char)((int)'a'+rand()%5));
    printf("\n");
    for (int i=2;i<=maxn;i++)
        printf("%d %d\n",i,rand()%(i-1)+1);
    printf("%d\n",maxm);
    for (int i=0;i<maxm;i++)
    for (int j=0;j<4;j++)
        printf("%d%c", rand()%maxn+1, j==3?'\n':' ');
}
