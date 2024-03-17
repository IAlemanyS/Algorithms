import math
import random

def factores(n,m):
	x = 3 * m - 2
	#s = math.ceil(math.sqrt(x))
	l = [0] * (x + 1)
	l[0] = 1
	l[1] = 1
	for i in range(2,x + 1):
		if l[i] == 0:
			j = 2*i
			while j < x + 1:
				l[j] = 1
				j += i
	return [i for i in range(0,x + 1) if l[i] == 0 and n % i == 0]

def lucas(N,m):
	if N == 2:
		return True
	if N == 1:
		return False
	l = factores(N - 1,m)
	for p in l:
		founda = False
		for a in range(2,N): 
			if pow(a,N-1,N) == 1 and pow(a,(N-1)//p,N) != 1:
				founda = True
				break
		if founda == False:
			return False #hemos probado todos los a's: no es primo
	return True

def  es_primo_rabin_miller(N, num_intentos):
	if N in { 2, 3, 5, 7 }:
		return  True
	if N < 11:
		return  False
	if N % 2 == 0:
		return  False
	r = 0
	m = N-1
	while m % 2 == 0:
		m //= 2
		r += 1
	for i in range(num_intentos):
		a = random.randint(1, N-1)
		if math.gcd(a, N) != 1:
			return  False
		x = pow(a, m, N)
		esta_en_T_N = (x == 1) or (x == N-1)
		t = 0
		while  not  esta_en_T_N  and t < r-1:
			x = pow(x, 2, N)
			esta_en_T_N = (x == N-1)
			t += 1
		if not  esta_en_T_N:
			return  False
	return True
	
def calculaN(m):
	N = 1
	for i in range(m):
		N *= 3*i + 1
	return N
	
def test_pm_plus_1(m):
	N = calculaN(m)
	if es_primo_rabin_miller(N + 1,1):
		print("p("+str(m)+") + 1 necesita Lucas...")
		x = lucas(N + 1,m)
		if x == True:
			print("p("+str(m)+") + 1 es primo por Lucas")
		else:
			print("p("+str(m)+") + 1 es compuesto por Lucas")
		return x
	else:
		print("p("+str(m)+") + 1 es compuesto por R-M")
		return False
		
