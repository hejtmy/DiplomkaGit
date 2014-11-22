library(binhf)
#helper function for switching and noSwitching
switching<-function(oldVec){
     #loads a vector of successive goals and shifts it left to compare
     newVec=shift(oldVec,1,dir='left')
     #compares the two vectors and tells us, whether the goals have changed in between the tasks
     changedGoal=!(newVec==oldVec)
     #returns a vector with T F for arena in the shifted vector
     whereNewArena=grepl("Arena",newVec)
     #simplifies the above vector : pust Arena instead of true and AI instead of false
     newVec=sapply(whereNewArena,FUN=function(x) if (x) return ('Arena') else return ('AI'))
     #returns a vector with T F for arena in the shifted vector
     whereOldArena=grepl("Arena",oldVec)
     #simplifies the above vector : pust Arena instead of true and AI instead of false
     oldVec=sapply(whereOldArena,FUN=function(x) if (x) return ('Arena') else return ('AI'))
     #puts the NA value at the end of the new shifted vector, as we will shift it back and it will pop to the beginning
     newVec[length(newVec)]<-NA
     #compares the two vectors and returns a logical vector which indicates whether the swhitch occred or not
     comp=newVec==oldVec
     #creates a new vector with information where the task switched to
     whereTo=shift(newVec,1)
     #shifts the outcome vectors
     comp=shift(comp,1,dir="right")
     changedGoal=shift(changedGoal,1,dir="right")
     changedGoal[1]<-NA
     #transforms the logical vector to an understandable code
     myList<-mapply(FUN=function(x,y) if (x) return(c('NOSWITCH',y)) else return(c('SWITCH',y)),comp[2:length(comp)],whereTo[2:length(whereTo)])
     list1=c(NA,myList[1,])
     list2=c(NA,myList[2,])
     return(list(list1,list2,changedGoal))
}

#helper fucntion for deciding correct answers
trefa <- function(table){
     
     #takes the angle difference between the subjects rotation and the rotation of the goal
     angleDiff=table[,abs(UhlovaVzdalenostCile)]
     #goal rotation
     angleGoal=table[,RotaceCile]
     #subjects rotation
     anglePlayer=table[,RotaceHrace]
     
     #angles of other goals
     otherAngles = table[,list(CilArena1Uhel,CilArena2Uhel,CilAI1Uhel,CilAI2Uhel)]
     
     diffs<-sapply(otherAngles,FUN=function(x) return(180-abs(abs(x - anglePlayer) - 180)))
     #finds the second lowest distance
     secLow<-sort(diffs,partial=2)[2]
     difference <- secLow - angleDiff
     
     #if the angle is lesser than 15 then we consider the answer automatically correct
     in.range=(angleDiff-15)<0
     if (in.range){
          return(list('CORRECT',difference))
     }
     
     if (difference+5<=0){
          return (list('INCORRECT',difference))
     }
     if (difference<=5){
          return (list('INCONCLUSIVE',difference))
     } 
     if (difference>=5){
          return (list('CORRECT',difference))
     }    
}

eraseAfterPause <-function(table){
     positions.differ=seq(21,120,by=21)
     positions.same=seq(21,166,by=21)
     columns_to_change=c("changedGoal","did.switch","whereTo")
     table[same.letters.x==T & Faze %in% positions.same,(columns_to_change):=NA]
     table[same.letters.x==F & Faze %in% positions.differ,(columns_to_change):=NA]
     return(table)
}

addSameColumn<-function(table){
     #adds a new column for future use
     table[,same.letters:=F]
     table[,id2:=as.numeric(id)]
     #log_table[,id2:=as.numeric(levels(log_table$id)[log_table$id])]
     table[id2>50,same.letters:=T]
     table[,id2:=NULL] 
     return(table)
}

addAngColumns<-function(table){
     #quite lengthy function to calculate the angular distance traveled
     table[,angDistTravel:=c(0,diff(HracRotaceY)),by=list(id,test.phase,Faze)]
     table[,angDistTravel:=(angDistTravel+180)%%360-180]
     #summation function
     table[,cumsumAngAbs:=cumsum(abs(angDistTravel)),by=list(id,test.phase,Faze)]
     
     #new columns describing the separate movemet left and right
     table[,leftAng:=0]
     table[angDistTravel<0,leftAng:=abs(angDistTravel)]
     table[,cumsumLeftAng:=cumsum(leftAng),by=list(id,test.phase,Faze)]
     table[,rightAng:=0]
     table[angDistTravel>0,rightAng:=abs(angDistTravel)]
     table[,cumsumRightAng:=cumsum(rightAng),by=list(id,test.phase,Faze)]
     return(table)
}

addKamilFaktor<-function(row){
     if(is.na(row[,did.switch])){
          return(0)
     }
     if(row[,did.switch]=='SWITCH'){
          return(3)
     }
     if(row[,changedGoal]==T){
          return(2)
     } else {
          return(1)
     }
     
}

addLogTail<-function(log_table,test_table){
     #creates small table from the log table with only the last log line 
     #(the one where the phase ended, therefor the one closest to the probands answer
     smallTable=rbind(log_table[,tail(.SD,1),by=list(id,test.phase,Faze)])
     #deletes the training data
     smallTable=smallTable[test.phase %in% c('F3','F4','F5')]
     setkey(smallTable,id,test.phase,Faze)
     setkey(test_table,id,test.phase,Faze)
     return(merge(test_table,smallTable,all.x=T))
}
                                         
addDistances<-function(log_table,test_table){
    
     #creates small table from the log table with only the first log line 
     #to compute some things
     smallTable=rbind(log_table[,head(.SD,1),by=list(id,test.phase,Faze)])
     
     #deletes the training data
     smallTable=smallTable[test.phase %in% c('F3','F4','F5')]
     
     #makes a merge between the test table and the smallTable2
     setkey(test_table,id,test.phase,Faze)
     setkey(smallTable,id,test.phase,Faze)
     
     helpTable=merge(test_table,smallTable,all.x=T)
     
     smallTable<-NULL
     
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
     return(merge(test_table,helpTable,all.x=T))
   
}

#adds the factor for same.letters condition
log_table<-addSameColumn(log_table)
test_table<-addSameColumn(test_table)

#list of angles
seznam=c('CilArena1Uhel','CilArena2Uhel','CilAI1Uhel','CilAI2Uhel')

log_table<-addAngColumns(log_table)

newTable<-addDistances(log_table,test_table)

newTable<-addLogTail(log_table,newTable)

#makes decisions about correct and incorrect answers
#has serious issues with R 3.1 and data.table 1.9.2 viz:http://r.789695.n4.nabble.com/internal-FALSE-TRUE-value-has-been-modified-td4689777.html
newTable[,c('correct.ans','diff.sec.closest'):=trefa(.SD),by=list(id,Faze,test.phase)]

#adds a reaction Time column
newTable[,reactionTime:=CasKliknuti-CasZadani]
newTable[,c('did.switch','whereTo','changedGoal'):=switching(.SD$JmenoCile),by=list(id,test.phase)]

#adds the quantities for correct.ans for individual subjects and phases
helpTable=newTable[correct.ans=="CORRECT", list(correctAnsQuant=nrow(.SD)),by=list(id,test.phase)]
setkey(helpTable,id,test.phase)
newTable=merge(newTable,helpTable)
#adds the quantities for correct.ans for individual subjects and phases
helpTable=newTable[,list(AnsQuant=nrow(.SD)),by=list(id,test.phase,correct.ans)]
setkey(helpTable,id,test.phase)
newTable=merge(newTable,helpTable,by=c("id","test.phase","correct.ans"),all.x=T)
setorder(newTable,id,test.phase,Faze)

newTable<-eraseAfterPause(newTable)

#adds the factor for the subsequent analysis
newTable[,kamilFaktor:=addKamilFaktor(.SD),by=list(id,test.phase,Faze)]
newTable[,kamilFaktor:=factor(kamilFaktor,levels=c(0,1,2,3),labels=c("NA","same.goal","dif.goal","dif.reference"))]

#minor corrections
setnames(newTable,c("same.letters.x","gender.x","date.time.x","date.x","age.x","zapis.x","exp.version.x","rozmisteni.x","time start.x"),c("same.letters","gender","date.time","date","age","zapis","exp.version","rozmisteni","time.start"))
newTable[,id:=factor(id)]
columnsToDelete=c("same.letters.y","gender.y","date.time.y","date.y","age.y","zapis.y","exp.version.y","rozmisteni.y","time start.y")
newTable[,(columnsToDelete):=NULL]