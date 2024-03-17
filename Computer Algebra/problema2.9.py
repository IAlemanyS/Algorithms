def parciales(l):
	p = [0]
	for i in range(len(l)):
		suma = 0
		for j in range(i+1):
			suma += l[j]
		p.append(suma)
	return p


def max_abs_sum_sublst(l):
	maxi = 0
	ini = 0
	fin = 1
	p = parciales(l)
	for j in range(1,len(p)):
		for i in range(0,j):
			if abs(p[j] - p[i]) > maxi:
				maxi = abs(p[j] - p[i])
				print("maximo:"+str(maxi) + " ini:"+str(i) + " fin:"+str(j))
				ini = i
				fin = j
	return (ini,fin)

#para que maximice sin valor absoluto:
def max_sum_sublst(l):
	maxi = 0
	ini = 0
	fin = 1
	p = parciales(l)
	for j in range(1,len(p)):
		for i in range(0,j):
			if p[j] - p[i] > maxi:
				maxi = p[j] - p[i]
				print("maximo:"+str(maxi) + " ini:"+str(i) + " fin:"+str(j))
				ini = i
				fin = j
	return (ini,fin)

