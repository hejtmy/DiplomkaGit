library(data.table)
library(binhf)

log_table[,max(Faze),by=(test.phase)]

test_table[,UhlovaVdalenostCile:=abs(UhlovaVdalenostCile)]
seznam=c('CilArena1Uhel','CilArena2Uhel','CilAI1Uhel','CilAI2Uhel')
#creates small table from the log table with only the last log line 
#(the one where the phase ended, therefor the one closest to the probands answer
smallTable=rbind(log_table[,tail(.SD,1),by=list(id,test.phase,Faze)])
#deletes the training data
smallTable=smallTable[test.phase %in% c('F3','F4','F5')]
#columnsToDelete = c('')
#smallTable[,.SD:=NULL,.SDcols=columnsToDelete]
#merges the information for future use
setkey(test_table,id,test.phase,Faze)
setkey(smallTable,id,test.phase,Faze)
newTable = merge(test_table,smallTable)
#deletes the help table
smallTable<-NULL

newTable[,c('correct.ans','diff.sec.closest'):=trefa(.SD),by=list(id,Faze,test.phase)]
table(newTable[,correct.ans,by=list(id,test.phase)])

#adds a reaction Time column
newTable[,reactionTime:=CasKliknuti-CasZadani]
newTable[,did.switch:=switching(.SD$JmenoCile),by=list(id,test.phase)]
newTable[correct.ans=='CORRECT',reactionTime,by=list(id,test.phase)]

useTable = newTable[correct.ans=='CORRECT',list(id,test.phase,Faze,JmenoOrientacnihoBodu,correct.ans,reactionTime,did.switch)]

bar = ggplot(useTable[did.switch != 'NA',list(mean=mean(reactionTime)),by=list(did.switch,test.phase)],aes(x=test.phase,y=mean,fill=did.switch))
bar+geom_bar(stat='identity',position='dodge')

useTable[did.switch != 'NA',list(sd=sd(reactionTime)),by=list(did.switch,test.phase)]

switching<-function(oldVec){
     newVec=shift(oldVec,1,dir='left')    
     newVec=grepl("Arena",newVec)
     oldVec=grepl("Arena",oldVec)
     newVec[length(newVec)]<-NA
     comp=newVec==oldVec
     comp=shift(comp,1,dir="right")
     comp[2:length(comp)]<-sapply(comp[2:length(comp)],FUN=function(x) if (x) return('NOSWITCH') else return('SWITCH'))
     return(comp)
}

trefa <- function(table){
     
     angleDiff=table[,UhlovaVdalenostCile]
     angleGoal=table[,RotaceCile]
     anglePlayer=table[,RotaceHrace]
     
     otherAngles = table[,list(CilArena1Uhel,CilArena2Uhel,CilAI1Uhel,CilAI2Uhel)]
     diffs<-sapply(otherAngles,FUN=function(x) return(180-abs(abs(x - anglePlayer) - 180)))
     #finds the second lowest distance
     secLow<-sort(diffs,partial=2)[2]
     difference <- secLow - angleDiff
     if (difference<=0){
          return (list('INCORRECT',difference))
     }
     if (difference<=5){
          return (list('INCONCLUSIVE',difference))
     } 
     if (difference>=5){
          return (list('CORRECT',difference))
     }
     
}