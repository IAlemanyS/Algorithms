#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

using namespace std;

bool resuelveCaso()
{
	int n;
	cin >> n;
	if (!std::cin)
		return false;
	int s;
	vector<int> tropas(n), enemigos(n);
	for (int i = 0; i < n; ++i)
	{
		cin >> s;
		enemigos[i] = s;
	}
	for (int i = 0; i < n; ++i)
	{
		cin >> s;
		tropas[i] = s;
	}
	int defendidas = 0;
	sort(enemigos.begin(), enemigos.end());
	sort(tropas.begin(), tropas.end());
	int t = 0;
	for (int e : enemigos)
	{
		while (t < n && tropas[t] < e)
			++t;
		if (t < n)
		{
			++defendidas;
			++t;
		}
		if (t >= n)
			break;
	}
	cout << defendidas << '\n';
	return true;
}

int main() 
{
	while (resuelveCaso());
	return 0;
}
