setwd("C:/Lukas/Vyzkum/Diplomka/DiplomkaGit/DiplomkaGit")
source(file="createSequence.R")
source(file="PsychopyColourSequencer.R")
source(file="PsychopyLeftRightSequencer.R")

createSimpleProband = function(i,n=1){
     #sets seed to mae it less random and retraceable
     set.seed(i)
     #id=?
     id=paste(c("id=",i),sep="",collapse="")
     #kod_jmena=?
     kodJ=paste(c("kod_jmena=",paste(sample(1:4,size=4),sep="",collapse="")),sep="",collapse="")
     #kod_barev=?
     kodB=paste(c("kod_barvy=",paste(sample(1:4,size=4),sep="",collapse="")),sep="",collapse="")
     #trenink1poradi=1111
     trenink1=paste(c("trenink1poradi=",createSequence(4,10,paste(c(sample(1:4,size=4),sample(1:4,size=4)),sep="",collapse=""))),sep="",collapse="")
#      trenink2poradi=2222
     trenink2=paste(c("trenink2poradi=",createSequence(4,10,paste(sample(1:4,size=4),sep="",collapse=""),T)),sep="",collapse="")
#      test1poradi=3333
     test1=paste(c("test1poradi=",createSequence(4,10)),sep="",collapse="")
#      test2poradi=4444
     test2=paste(c("test2poradi=",createTurnSequence(4,10,samePair=T)),sep="",collapse="")
#      test3poradi=555555555
     test3=paste(c("test3poradi=",createTurnSequence(4,10,samePair=T)),sep="",collapse="")
#      pauza_po=7   
     pauza="pauza_po=20"
#      zobrazit_cil_po=5
     zobrazit="zobrazit_cil_po=10"
#      zapisovat_od_zacatku=0
     zapisovat = "zapisovat_od_zacatku=0"

     typE = paste(c("typ_experimentu=",n),sep="",collapse="")

     #creates file connection
     inif<-file(paste(c(i,"-",n,".ini"),sep="",collapse=""))
     writeLines(c(id,kodJ,kodB,trenink1,trenink2,test1,test2,test3,pauza,zobrazit,typE,zapisovat),inif,sep="\n")
     close(inif)
if (n==1){
     #create colour files for Psychopy
     expfc <- file(paste(c(i,"-colour.csv"),sep="",collapse=""))
     table <- createColourExp(kodJ,kodB)
     write.csv(table,expfc,quote=F,row.names=F)
}
#create left-right files for psyhcopy
if (n==1){
     expflr <- expfc <- file(paste(c(i,"-leftright.csv"),sep="",collapse=""))
     tablelr <- createSequenceLeftRight(i)
     write.csv(tablelr,expflr,quote=F,row.names=F)
}

}

for (i in listOfPossibUsers){
     for (n in 1:4){
          createSimpleProband(i,n)
     }
}

for (i in listOfPossibUsers){
     expflpr <- file(paste(c(i,"-pureleftright.csv"),sep="",collapse=""))
     tablelr <- createSequencePureLeftRight(i)
     write.csv(tablelr,expflpr,quote=F,row.names=F)
}

for (i in listOfPossibUsers){
     expflfb <- file(paste(c(i,"-frontback.csv"),sep="",collapse=""))
     tablelr <- createSequenceFrontBack(i)
     write.csv(tablelr,expflfb,quote=F,row.names=F)
}

listOfPossibUsers = c(51,52,53)

createSimpleProband(109,1)
for(i in 170:400){
     try(createSimpleProband(i,1),silent=T)
}
