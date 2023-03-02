#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
using namespace std;

bool resuelveCaso() 
{
	int n, L;
	cin >> n >> L;
	if (!std::cin)
		return false;
	vector<long long> nformas(L + 1);
	vector<long long> mincordeles(L + 1);
	vector<long long> mincoste(L + 1);
	vector<long long> l(n + 1);
	vector<long long> c(n + 1);
	l[0] = 0; c[0] = 0;
	for (int i = 1; i <= n; ++i)
		cin >> l[i] >> c[i];
	for (int j = 1; j <= L; ++j)
	{
		nformas[j] = 0;
		mincordeles[j] = -1; // infty
		mincoste[j] = -1; // infty
	}
	nformas[0] = 1;
	mincordeles[0] = 0;
	mincoste[0] = 0;
	for (int i = 1; i <= n; ++i)
	{
		for (int j = L; j > 0; --j)
		{
			if (j >= l[i]) // en caso contrario no varían
			{
				nformas[j] = nformas[j] + nformas[j - l[i]];
				if (mincordeles[j] == -1)
				{
					if (mincordeles[j - l[i]] != -1) // en caso contrario se queda valiendo -1
						mincordeles[j] = mincordeles[j - l[i]] + 1;
				}
				else if (mincordeles[j - l[i]] != -1)
					mincordeles[j] = min(mincordeles[j], mincordeles[j - l[i]] + 1);
				if (mincoste[j] == -1)
				{
					if (mincoste[j - l[i]] != -1) // en caso contrario se queda valiendo -1
						mincoste[j] = mincoste[j - l[i]] + c[i];
				}
				else if (mincoste[j - l[i]] != -1)
					mincoste[j] = min(mincoste[j], mincoste[j - l[i]] + c[i]);
			}
		}
	}

	if (nformas[L] == 0)
		cout << "NO\n";
	else
		cout << "SI " << nformas[L] << " " << mincordeles[L] << " " << mincoste[L] << '\n';
	return true;
}

int main() 
{
	while (resuelveCaso());
	return 0;
}
