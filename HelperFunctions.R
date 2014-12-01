#function converting gtoal name to letter 
makeletter<-function(cile,kodJ){
     
     letters = c("A","B","C","D")
     kodJ = strsplit(kodJ,"",fixed=T)[[1]]
     pismena <- c()
     for (i in kodJ){
          pismena <- c(pismena,letters[as.integer(i)])
     }
     goals<-c()
     for (n in cile){
          if(n=="CilArena1") i = 1
          if(n=="CilArena2") i = 2
          if(n=="CilAI1") i = 3
          if(n=="CilAI2") i = 4  
          goals=c(goals,pismena[i])
     }
     return(goals)
}

newTable[,partialCode:=strsplit(rozmisteni,"-")[[1]][2],by=newid]
newTable[,letter:=makeletter(.SD$JmenoCile,.SD$partialCode),by=newid]
