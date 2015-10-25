mojeFunkce <- function(vector){
     newVec=rep(0,length(vector))
     for(i in 2:length(vector)){
          if(!is.na(vector[i])){
               if(vector[i]=="same.goal"){
                    newVec[i]=newVec[i-1]+1
               }
          } 
          
          if(is.na(vector[i-1])){
               newVec[i-1]=NA
          }
     }
     
     return(newVec)
     
}

newTable[,numRep:=mojeFunkce(.SD$ExpFaktor),by=list(newid,test.phase)]
newTable[,numRep:=factor(numRep)]
head(newTable[,numRep])
newOne=newTable[ExpFaktor=="same.goal",mean(rtCorrected,na.rm=T),by=numRep]

summaryTable<-newTable[ExpFaktor=="same.goal",list(mean=mean(rtCorrected,na.rm=T),sd=sd(rtCorrected,na.rm=T)),by=numRep]

plot=ggplot(newTable[ExpFaktor=="same.goal" & rtCorrected < 4,],aes(x=numRep,y=rtCorrected))
plot+geom_boxplot() + xlab("Repetitions") + ylab("Reaction Times")

fit<-aov(rtCorrected~numRep,newTable[ExpFaktor=="same.goal" & rtCorrected < 4,])
anova(fit)

model.all=ezANOVA(
     data=newTable[ExpFaktor=="same.goal",],
     dv=rtCorrected,
     wid=newid,
     within=numRep,
     return_aov=T,
     detailed=T,
)

ezDesign(
     data=newTable[ExpFaktor=="same.goal",], 
     x=newid,
     y=numRep
)
