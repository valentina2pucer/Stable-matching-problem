from random import shuffle
import time

def preference(n):
    #funkcija preference nam zgenerira naključen seznam preferenc vsakega moškega in vsake ženske
    moski = []
    zenske = []
    for i in range(n):
        M = [j for j in range(n)]
        shuffle(M)
        moski.append(M)
        Z = [j for j in range(n)]
        shuffle(Z)
        zenske.append(Z)
    return moski, zenske

def stable_marriage(moski,zenske):
    #Funkcija vsebuje ILP za naš problem, vstopni podatki, sta seznama preferenc za moške in ženske
    n = len(moski)
    pari=[]
    p = MixedIntegerLinearProgram(maximization = True)
    x = p.new_variable(binary = True)
    p.set_objective(sum(sum(x[i,j] for j in range(n)) for i in range(n)))
    for i in range(n):
        p.add_constraint(sum(x[i,j] for j in range(n))== 1)
    for j in range(n):
        p.add_constraint(sum(x[i,j] for i in range(n))== 1)
    for u, a in enumerate(moski):
        for v, b in enumerate(zenske):
            p.add_constraint(sum(x[u, i] for i in a[:a.index(v)]) + sum(x[i, v] for i in b[:b.index(u)]) + x[u, v] >= 1)
    #za vsakega moškega u dobimo njegov seznam a, za vsako žensko v pa njen seznam b, a.index(v) bo torej določal           zgornjo mejo pri moških->višja prioriteta tistih žensk, ki so na začetku (zapis pove, da ali sta moški in ženska       skupaj, če nista pomeni, da sta z nekom, ki ga bolj preferirata)
    p.solve()
    s = p.get_values(x) #slovar parov (0 nista porocena, 1 sta)
    for key, value in s.items():
        if value==1: pari.append(key)
    return pari

#Naredimo funkcijo, ki nam meri čas algoritma v sekundah
#Funkcija nam za neko velikost 10x za različne podatke izračuna čas in na koncu vrne povprečen čas
def cas_ILP(n):
    casi = []
    for i in range(10):
        tabeli = preference(n)
        moski = tabeli[0]
        zenske = tabeli[1]
        start = time.time()
        stable_marriage(moski,zenske)
        end = time.time()
        t = end - start
        casi.append(t)
    povp = sum(casi)/10
    return povp

#Sedaj naredimo novo funkcijo, ki nam za določene n-je izračuna čas in podatke shrani v tabelo, saj bi radi potem podatke uvozili v R, kjer bomo primerjali algoritem ILP z algoritmom, ki ga imamo napisanega v R.

velikost = [10+x*10 for x in range(0, 10)]
cas = []
for i in velikost:
    cas.append(cas_ILP(i))

#Spremenimo naš seznam cas v vektor, ki ga bomo lahko skopirali direktno v R za primerjavo časov med algoritmoma
Rfloatlist = lambda l: "c(%s)" % ', '.join("%f" % x for x in l)
print(Rfloatlist(cas))









