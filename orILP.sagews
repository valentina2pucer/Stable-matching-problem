︠1358fe84-0a7c-483e-bd75-912fc8f6cde0r︠
p = MixedIntegerLinearProgram(maximization = True)
x = p.new_variable(binary = True)

import timeit

start = timeit.default_timer()
t=cputime()

from random import shuffle
n=20

#n=5
moski = []
zenske = []
pari=[]
for i in range(n):
    M = [j for j in range(n)]
    shuffle(M)
    moski.append(M)
    Z = [j for j in range(n)]
    shuffle(Z)
    zenske.append(Z)
#print(moski) #zgenerirali smo naključen seznam preferenc vsakega moškega in vsake ženske
#print(zenske)

p.set_objective(sum(sum(x[i,j] for j in range(n)) for i in range(n)))

for i in range(n):
    p.add_constraint(sum(x[i,j] for j in range(n))== 1)
for j in range(n):
    p.add_constraint(sum(x[i,j] for i in range(n))== 1)


for u, a in enumerate(moski):
    for v, b in enumerate(zenske):
        p.add_constraint(sum(x[u, i] for i in a[:a.index(v)]) + sum(x[i, v] for i in b[:b.index(u)]) + x[u, v] >= 1)
#za vsakega moškega u dobimo njegov seznam a, za vsako žensko v pa njen seznam b, a.index(v) bo torej določal zgornjo mejo pri moških->višja prioriteta tistih žensk, ki so na začetku

p.solve()

s = p.get_values(x) #slovar parov (0 nista porocena, 1 sta)
#print(s)
for key, value in s.items():
    if value==1: pari.append(key)

stop = timeit.default_timer()
cputime(t)
print(stop-start)

print(pari) #print stabilnih parov










