library(binhf)
#helper function for switching and noSwitching
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

#helper fucntion for deciding correct answers
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

#adds a new column for future use
log_table[,same.letters:=F]
log_table[,id2:=as.numeric(id)]
#log_table[,id2:=as.numeric(levels(log_table$id)[log_table$id])]
log_table[id2>50,same.letters:=T]
log_table[,id2:=NULL]

test_table[,same.letters:=F]
#test_table[,id2:=as.numeric(levels(test_table$id)[test_table$id])]
test_table[,id2:=as.numeric(id)]
test_table[id2>50,same.letters:=T]
test_table[,id2:=NULL]

#makes each angle difference an absolute value
test_table[,UhlovaVzdalenostCile:=abs(UhlovaVzdalenostCile)]
#list of angles
seznam=c('CilArena1Uhel','CilArena2Uhel','CilAI1Uhel','CilAI2Uhel')

#quite lengthy function to calculate the angular distance traveled
log_table[,angDistTravel:=c(0,diff(HracRotaceY)),by=list(id,test.phase,Faze)]
log_table[,angDistTravel:=(angDistTravel+180)%%360-180]
#summation function
log_table[,cumsumAngAbs:=cumsum(abs(angDistTravel)),by=list(id,test.phase,Faze)]

#new columns describing the separate movemet left and right
log_table[,leftAng:=0]
log_table[angDistTravel<0,leftAng:=abs(angDistTravel)]
log_table[,cumsumLeftAng:=cumsum(leftAng),by=list(id,test.phase,Faze)]
log_table[,rightAng:=0]
log_table[angDistTravel>0,rightAng:=abs(angDistTravel)]
log_table[,cumsumRightAng:=cumsum(rightAng),by=list(id,test.phase,Faze)]

#creates small table from the log table with only the last log line 
#(the one where the phase ended, therefor the one closest to the probands answer
smallTable=rbind(log_table[,tail(.SD,1),by=list(id,test.phase,Faze)])

#deletes the training data
smallTable=smallTable[test.phase %in% c('F3','F4','F5')]

#creates small table from the log table with only the first log line 
#to compute some things
smallTable2=rbind(log_table[,head(.SD,1),by=list(id,test.phase,Faze)])

#deletes the training data
smallTable2=smallTable2[test.phase %in% c('F3','F4','F5')]

#makes a merge between the test table and the smallTable2
setkey(test_table,id,test.phase,Faze)
setkey(smallTable2,id,test.phase,Faze)
helpTable=merge(smallTable2,test_table)
smallTable2<-NULL
#creates two new variables to use for control
helpTable[,cilUhelZadani:=get(paste(c(JmenoCile,"Uhel"),sep="",collapse="")),by=list(id,Faze,test.phase)]
#distance1 is the angular distance between the goal nad the player rotation when the goal is given
helpTable[,distance1:=((cilUhelZadani-HracRotaceY+180)%%360-180)]
#distance2 is the angular distance between the player rotation when the goal is given and the 
#position of th eogal when the answer is given
helpTable[,distance2:=((RotaceCile-HracRotaceY+180)%%360-180)]

helpTable=helpTable[,list(Faze,id,test.phase,distance1,distance2)]
setkey(helpTable,id,test.phase,Faze)
#columnsToDelete = c('')
#smallTable[,.SD:=NULL,.SDcols=columnsToDelete]

#merges the information for future use
setkey(test_table,id,test.phase,Faze)
setkey(smallTable,id,test.phase,Faze)
newTable = merge(test_table,smallTable)
#adds the distances one and two
newTable=merge(newTable,helpTable)
#deletes help tables
smallTable<-NULL
helpTable<-NULL

#makes decisions about correct and incorrect answers
#has serious issues with R 3.1 and data.table 1.9.2 viz:http://r.789695.n4.nabble.com/internal-FALSE-TRUE-value-has-been-modified-td4689777.html
newTable[,c('correct.ans','diff.sec.closest'):=trefa(.SD),by=list(id,Faze,test.phase)]

#adds a reaction Time column
newTable[,reactionTime:=CasKliknuti-CasZadani]
newTable[,c('did.switch','whereTo'):=switching(.SD$JmenoCile),by=list(id,test.phase)]

useTable = newTable[correct.ans!='INCORRECT',list(id,test.phase,Faze,JmenoOrientacnihoBodu,JmenoCile,correct.ans,reactionTime,did.switch,whereTo,cumsumAngAbs,same.letters.x,distance1,distance2)]
useTable[,pokus:=(reactionTime)/cumsumAngAbs]
#testing purposes
#oldVec = newTable[test.phase=="F3",JmenoCile]
