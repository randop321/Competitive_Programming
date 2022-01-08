#include<map>
#include<set>
#include<cstdio>
#include<cstring>
#include<vector>
#include<iostream>
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
const int base[2] = {307,521};
const int mod[2] = {1000000007, 900000011};
char st[maxn];
int dep[maxn];
int up[maxn][2], down[maxn][2];
int pow[maxn][2], inv[maxn][2];
vi e[maxn], q;
int n,m,num;
int dfn[maxn*2][21];
int node2dfn[maxn];
int lg2[maxn*2];
int cnt[maxn];
vector<vi> chain;
pii node2chain[maxn];
int top[maxn];
int pre[maxn];
int h[maxn][6];

int fastpow(int a, int b, int c)
{
    int res = 1;
    while (b)
    {
        if (b&1) res = (ll)res*a%c;
        a = (ll)a*a%c;
        b/=2;
    }
    return res;
}

void dfs(int u,int prev)
{
    dfn[++num][0]=u;
    node2dfn[u] = num;
    cnt[u] = 1;
    dep[u] = dep[prev] + 1;
    pre[u] = prev;
    for (int j=0;j<2;j++)
    {
        up[u][j] = ((ll)up[prev][j]*base[j]+st[u])%mod[j];
        down[u][j] = (down[prev][j] + (ll)pow[dep[u]-1][j]*st[u])%mod[j];
    }
    for (auto v: e[u])
    if (!dep[v])
    {
        dfs(v, u);
        dfn[++num][0] = u;
        cnt[u] += cnt[v];
    }
}

void dfs2(int u)
{
    if (!top[u])
    {
        vi tmp;
        tmp.pb(u);
        for (int i=u;;i=tmp[tmp.size()-1])
        {
            int w = -1;
            for (auto j: e[i])
            if ((dep[j] > dep[i]) && ((w==-1) || (cnt[w] < cnt[j])))
                w = j;
            if (w==-1) break;
            else tmp.pb(w);
        }
        chain.pb(tmp);
        for (int i=0; i<tmp.size(); i++)
        {
            node2chain[tmp[i]] = mp(chain.size()-1, i);
            top[tmp[i]] = tmp[0];
            h[tmp[i]][0] = pre[top[tmp[i]]];
        }
        for (int k=1;k<6;k++)
        for (auto i:tmp)
            h[i][k] = h[h[i][k-1]][k-1];
    }
    for (auto v:e[u])
    if (dep[v] > dep[u])
        dfs2(v);
}

int find_lca(int u, int v)
{
    int uu = node2dfn[u], vv = node2dfn[v];
    if (uu>vv) swap(uu,vv);
    int w = lg2[vv-uu+1];
    int a = dfn[uu][w], b = dfn[vv-(1<<w)+1][w];
    if (dep[a] < dep[b]) return a;
    return b;
}

int find_kth_pre(int u, int k)
{
    if (!k) return u;
    int v = u;
    for (int i=5;i>=0;i--)
    if (dep[h[v][i]] >= dep[u]-k)
        v = h[v][i];
    pii tmp = node2chain[v];
    return chain[tmp.first][tmp.second - (dep[v] - dep[u] + k)];
}
    

void get_hash(int u, int v, int lca, int len, int k[])
{
    k[0]=k[1]=0;
    if (len <= dep[u] - dep[lca]+1)
    {
        int v = find_kth_pre(u, len-1);
        for (int i=0;i<2;i++)
        {
            k[i] = up[u][i] - (ll)up[pre[v]][i]*pow[len][i]%mod[i];
            if (k[i] < 0) k[i] += mod[i];
        }
        return;
    }
    for (int i=0;i<2;i++)
    {
        k[i] = up[u][i] - (ll)up[pre[lca]][i] * pow[dep[u]-dep[lca]+1][i]%mod[i];
        if (k[i] < 0) k[i] += mod[i];
    }
    len -= dep[u] - dep[lca]+1;
    if (!len) return;
    int w = dep[lca] + len;
    v = find_kth_pre(v, dep[v] - w);
    for (int i=0;i<2;i++)
    {
        int tmp = (ll)(down[v][i] - down[lca][i])*inv[dep[lca]][i]%mod[i];
        tmp  = (ll)tmp*pow[dep[u]-dep[lca]+1][i]%mod[i];
        if (tmp<0) tmp += mod[i];
        k[i] += tmp;
        if (k[i] >= mod[i]) k[i] -= mod[i];
    }
}

int main()
{
    //freopen("inp.txt","r", stdin);
   
    scanf("%d",&n);
    scanf("%s",st+1);
    for (int i=1,u,v;i<n;i++)
    {
        scanf("%d%d",&u,&v);
        e[u].pb(v);e[v].pb(u);
    }
    
    pow[0][0]=pow[0][1]=1;
    for (int i=1;i<=n;i++)
    for (int j=0;j<2;j++)
        pow[i][j] = (ll)pow[i-1][j]*base[j]%mod[j];
    inv[0][0] = inv[0][1] = 1;
    inv[1][0] = fastpow(base[0], mod[0]-2, mod[0]);
    inv[1][1] = fastpow(base[1], mod[1]-2, mod[1]);
    for (int i=2;i<=n;i++)
    for (int j=0;j<2;j++)
        inv[i][j] = (ll)inv[i-1][j]*inv[1][j]%mod[j];
    lg2[1] = 0;
    for (int i=2;i<=n*2;i++)
        lg2[i] = (1<<(lg2[i-1]+1))<= i ? lg2[i-1]+1 : lg2[i-1];
        
    dfs(1,0);
    dfs2(1);
    for (int i=1;(1<<i)<=num;i++)
    for (int j=1;j+(1<<i)-1<=num;j++)
    {
        int u = dfn[j][i-1];
        int v = dfn[j+(1<<(i-1))][i-1];
        dfn[j][i] = dep[u] < dep[v] ? u : v;
    }
    
    scanf("%d",&m);
    for (int i=0,s0,t0,s1,t1,lca0,lca1;i<m;i++)
    {
        scanf("%d%d%d%d",&s0,&t0,&s1,&t1);
        //printf("%d %d %d %d %d\n", i, s0,t0,s1,t1);
        //if (i==49339)
        //    printf("\n");
        if (st[s0] != st[s1])
        {
            printf("0\n");
            continue;
        }
        
        lca0 = find_lca(s0,t0);
        lca1 = find_lca(s1,t1);
        if (dep[s0]-dep[lca0] > dep[s1]-dep[lca1])
        {
            swap(s0, s1);
            swap(t0, t1);
            swap(lca0, lca1);
        }
        
        int l=0, r=min(dep[s0]+dep[t0]-2*dep[lca0]+1, dep[s1]+dep[t1]-2*dep[lca1]+1)+1;
        while (l+1<r)
        {
            int mid = (l+r)/2;
            int k0[2], k1[2];
            get_hash(s0,t0,lca0,mid,k0);
            get_hash(s1,t1,lca1,mid,k1);
            if (k0[0]==k1[0] && k0[1]==k1[1])
                l = mid;
            else r = mid;
        }
        printf("%d\n", l);
    }   
	return 0;
}
