library(binhf)

switching<-function(oldVec){
     newVec=shift(oldVec,1,dir='left')    
     whereNewArena=grepl("Arena",newVec)
     newVec=sapply(whereNewArena,FUN=function(x) if (x) return ('Arena') else return ('AI'))
     whereOldArena=grepl("Arena",oldVec)
     oldVec=sapply(whereOldArena,FUN=function(x) if (x) return ('Arena') else return ('AI'))
     newVec[length(newVec)]<-NA
     comp=newVec==oldVec
     whereTo=shift(newVec,1)
     comp=shift(comp,1,dir="right")     
     myList<-mapply(FUN=function(x,y) if (x) return(c('NOSWITCH',y)) else return(c('SWITCH',y)),comp[2:length(comp)],whereTo[2:length(whereTo)])
     
     list1=c(NA,myList[1,])
     list2=c(NA,myList[2,])
     return(list(list1,list2))
}

trefa <- function(table){
     
     angleDiff=table[,UhlovaVzdalenostCile]
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

test_table[,UhlovaVzdalenostCile:=abs(UhlovaVzdalenostCile)]
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
newTable[,c('did.switch','whereTo'):=switching(.SD$JmenoCile),by=list(id,test.phase)]

useTable = newTable[correct.ans=='CORRECT',list(id,test.phase,Faze,JmenoOrientacnihoBodu,correct.ans,reactionTime,did.switch,whereTo)]

#testing purposes
#oldVec = newTable[test.phase=="F3",JmenoCile]
