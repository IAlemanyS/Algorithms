#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cmath>
using namespace std;

bool resuelveCaso() 
{
	int n, pmin, pmax;
	cin >> n >> pmax >> pmin;
	if (!std::cin)
		return false;
	vector<float> p(n); // potencias
	vector<float> c(n); // costes
	vector<float> mincoste(pmax + 1);
	for (int i = 0; i < n; ++i)
		cin >> p[i];
	for (int i = 0; i < n; ++i)
		cin >> c[i];
	for (int j = 1; j <= pmax; ++j)
		mincoste[j] = INFINITY;
	mincoste[0] = 0;
	for (int i = 1; i <= n; ++i)
		for (int j = 1; j <= pmax; ++j)
			if (j >= p[i - 1])
				mincoste[j] = min(mincoste[j], mincoste[j - p[i - 1]] + c[i - 1]); // i-1 porque c y p van de 0 a n-1
	float sol = INFINITY;
	int indsol = 0;
	for (int j = pmin; j <= pmax; ++j)
	{
		if (mincoste[j] < sol)
		{
			sol = mincoste[j];
			indsol = j;
		}
	}
	if (sol == INFINITY)
		cout << "IMPOSIBLE\n";
	else
		cout << sol << ' ' << indsol << '\n';
	return true;
}

int main() 
{
	while (resuelveCaso());
	return 0;
}
