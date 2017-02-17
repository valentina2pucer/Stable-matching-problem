############## GALE SHAPLEY-ev ALGORITEM ##############

#slučajno zgeneriramo matriki preferenc glede na n
preference <- function(n){
  M <<- replicate(n,sample(1:n))
  Z <<- replicate(n,sample(1:n))
  n <<- n #shranimo n
}

GSA <- function(n=n,mPref=M,zPref=Z){
  #v funkcijo vpišemo število moških in žensk (ki je enako) ter matriki preferenc 
  m.samski <- 1:n #Moški, ki so samski (na začetku so vsi)
  zarocenke <- rep(0,n)
  m.zgodovina <- rep(0,n) #Število žensk, ki jih zaprosi moški
  while (length(m.samski) !=0){
    for (j in m.samski){
      m.zgodovina[j] <- m.zgodovina[j] + 1 #Zasprosi eno več kot prej
      zaprosena <- mPref[m.zgodovina[j],j] #j-ti moški v samskih zaprosi žensko, ki jo naslednjo preferira
      if (zarocenke[zaprosena] ==0){
        zarocenke[zaprosena] <- j #če je zaprošena ženska samska, potem se zaroči s moškim, ki jo je zaprosil
        m.samski <- m.samski[-match(j, m.samski)] #odstranimo j-tega moškega iz seznama samskih
      }
      else if (match(j,zPref[,zaprosena]) < match(zarocenke[zaprosena], zPref[,zaprosena])){
        #Če je vrednost j-tega moškega pri zaprošeni ženski višja kot vrednost s katerim je trenutno zaročena, 
        #zapusti trenutnega moškega in se zaroči z j-tim
        zapusceni <- zarocenke[zaprosena] #moški, ki ga bo zapustila
        zarocenke[zaprosena] <- j #nov zaročenec
        #odstranimo j-tega moškega iz seznama samskih
        m.samski <- m.samski[-match(j, m.samski)]
        #dodamo moškega, ki ga je zapustila v seznam samskih
        m.samski <- c(m.samski,zapusceni)
      }
    }
  }
  pari <- data.frame("Moški"=zarocenke,"Ženska"=1:n)
  return(list(mPref=mPref,zPref=zPref,ujemanje=pari))
}





############## ČAS ALGORITMA ##############


#Čas algoritma GSA, glede na velikost podatkov

cas_GSA <- function(n){
  #gledali bomo, koliko casa porabimo za izvedbo glede na velikost matrike
  #Čas bomo za vsak n izračunali 10x in vzeli povprečje
  cas <- c()
  for (i in 1:10){
    preference(n)
    start <- proc.time()[1]
    GSA(n,M,Z)
    end <- proc.time()[1]
    cas <- c(cas,end-start)
  }
  cas_povp <- sum(cas)/10
  return(cas_povp)
}

############## RISANJE GRAFA ##############

velikost <- seq(10, 1000, 50)
cas <- c()
for (i in velikost){
  cas <- c(cas,cas_GSA(i))
}

require(dplyr)
require(ggplot2)
podatki <- data_frame(velikost,cas)

ggplot(podatki,
       aes(x =velikost,y=cas)) + 
  geom_point() + geom_line() + 
  xlab("Število oseb posameznega spola")+
  ylab("Čas (v sekundah)") + 
  ggtitle("Čas algoritma, glede na število ljudi na posamezen spol")
