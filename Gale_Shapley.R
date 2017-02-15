
preference <- function(n){
  #funkcija nam vrne dve matriki s preferencami, ena za moške, ena za ženske
  #vnesemo število moških in žensk, ki je enako (n)
  mPref <<- replicate(n,sample(seq(1,n,1)))
  zPref <<- replicate(n,sample(seq(1,n,1)))
  #Poimenujemo stolpce in vrstice
  colnames(mPref) <<- paste0("M",1:n)
  rownames(mPref) <<- rep("",n)
  colnames(zPref) <<- paste0("Z",1:n)
  rownames(zPref) <<- rep("",n)
  #Shranimo še n za kasnejšo rabo:
  n <<- n
}

preference(10)

require(timeit)
start.time<-Sys.time()

GSA <- function(n,mPref,zPref){
  m.prej    <- rep(0,n)	# število prošenj, ki so bile narejene
  z.prej    <- rep(0,n)	# trenutni partner
  m.samski <- 1:n
  z.samski <- 1:n
  while (length(m.samski)!=0){	
    #največ možnih krogov bo toliko, kolikor je število žensk (n)
    #(če je en moški zavrnjen od vsake)
    #Za vsakega samskega moškega gledamo koga je že zaprosil in zaprosi naslednjo po vrsti
    prosnje <- NULL
    for (i in 1:length(m.samski)){
      m.prej[m.samski[i]] <- m.prej[m.samski[i]]+1	#zaprosi eno več
      prosnje[i] <- mPref[m.prej[m.samski[i]],m.samski[i]]		#  #Vrednost, ki ji imajo moški pri k-ti ženski (offer if single i is the index of the woman corresponding to current round
    }
    zaprosene   <- unique(prosnje)	#Ženske, katere so bile zaprošene
    samski <- m.samski
    m.samski    <- NULL
    for (j in zaprosene){
      zaprositelji   <- samski[prosnje==j]
      for (k in 1:length(zaprositelji)){
        if (z.prej[j]==0){	# če trenutno nima moškega
          z.prej[j] <- zaprositelji[k]
        }else if (match(zPref[zPref[ ,j]==zaprositelji[k],j],zPref[ ,j])<match(zPref[zPref[ ,j]==z.prej[j],j],zPref[ ,j])){
          m.samski <- c(m.samski,z.prej[j])		# če je tisti ki zaproša boljši, kot tisti s katerim je zaročena, naj zapusti trenutnega
          z.prej[j] <- zaprositelji[k]	#zaroči se z novim
        } else {
          m.samski <- c(m.samski,zaprositelji[k])	# če ni boljši, moški ostane samski
        }
      }	
    }
    if (length(m.samski)==0){	# če ni več nobenega samskega se ustaviš
      pari <- data.frame("Moški"=z.prej,"Ženska"=1:n)
      return(list(mPref=mPref,zPref=zPref,ujemanje=pari))
    }
  }
}
end.time<-Sys.time()
time.taken <- end.time - start.time
time.taken

