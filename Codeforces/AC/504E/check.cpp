#include<map>
#include<set>
#include<cstdio>
#include<vector>
#include<iostream>
#include<string>
#include<algorithm>
using namespace std;

typedef pair<int,int> pii;
typedef map<int,int> mii;
typedef set<int> si;
typedef vector<int> vi;
typedef long long ll;
typedef long double ld;
#define pb push_back
#define mp make_pair

const int maxn=300000+10;
char st[maxn];
int dep[maxn];
vi e[maxn], q;
int n,m,num;
vector<char> a,b;


bool dfs(int u,int v,int prev, vector<char> &s)
{
    if (u==v) return 1;
    for (auto w:e[u])
    if (w!=prev)
    {
        s.pb(st[w]);
        bool tmp = dfs(w,v,u,s);
        if (tmp) return 1;
        s.pop_back();
    }
    return 0;
}

int main()
{
    //freopen("1.in","r", stdin);
   
    scanf("%d",&n);
    scanf("%s",st+1);
    for (int i=1,u,v;i<n;i++)
    {
        scanf("%d%d",&u,&v);
        e[u].pb(v);e[v].pb(u);
    }
    
    
    scanf("%d",&m);
    for (int i=0,s0,t0,s1,t1,lca0,lca1;i<m;i++)
    {
        scanf("%d%d%d%d",&s0,&t0,&s1,&t1);
        a.clear();
        b.clear();
        a.pb(st[s0]);
        b.pb(st[s1]);
        dfs(s0, t0, -1, a);
        dfs(s1, t1, -1, b);
        int l=0;
        while (l < a.size() && l < b.size() && a[l] == b[l]) l++;
        printf("%d\n", l);
    }   
	return 0;
}
