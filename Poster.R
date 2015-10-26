setwd("U:/Vyzkum/Diplomka/DiplomkaGit")
#setwd("c:/users/luká/vyzkum/diplomka/diplomkagit")
library(ggplot2)
library(data.table)
library(plyr)
library(ez)
library(car)
#loads up the table
newTable=fread("Data/newTableVerAll3.0.txt",sep=";",header=T,autostart=1)
#remove one group - too small
newTable=newTable[!(same.letters==F & exp.version==2)]
# ---------------
# Does simple preprocessing
# ---------------
newTable[,newid:=factor(newid)]
newTable[,same.letters:=factor(same.letters,levels=c(T,F),labels=c("full.alternation","partial.alternation"))]
newTable[,ExpFaktor:=factor(kamilFaktor,levels=c("same.goal","dif.goal","dif.reference","NA"),labels=c("same.goal","different.goal","different.reference.frame","NA"))]
newTable[,kamilFaktor:=NULL]
newTable[,exp.version:=factor(exp.version)]
setkey(newTable,newid,exp.version,test.phase,Faze)
newTable[,rtCorrected:=reactionTime-meanLetterRT]
newTable[,UhlovaVzdalenostCileAbs:=abs(UhlovaVzdalenostCile)]


setorder(newTable,newid,test.phase,Faze)
newTable[,percentAns:=.SD$AnsQuant/nrow(.SD),by=list(newid,test.phase)]

#use table is only for correct answers, when the goal was changed and the answer was relatively fast
useTable = newTable[correct.ans=='CORRECT' & changedGoal!="NA" & reactionTime<5]
#selects only those people who had more then 75 percend of correct answers
smallFrame=unique(newTable[,list(newid,test.phase)])
correct.ones=unique(newTable[correct.ans=='CORRECT' & percentAns>0.75, list(newid,test.phase)])
useTableCor=merge(useTable,correct.ones,by=c("newid","test.phase"))

#setting ggplot
theme_set(theme_bw(base_size = 25))
update_geom_defaults("line",   list(size= 1))
dodge <- position_dodge(width=0.9)

doAnovaThingsUhel<-function(subset){
     stats=ezStats(
          data=subset,
          dv=UhlovaVzdalenostCileAbs,
          wid=newid,
          within=c(ExpFaktor,whereTo)
     )
     
     statsGender=ezStats(
          data=subset,
          dv=UhlovaVzdalenostCileAbs,
          wid=newid,
          within=c(ExpFaktor,whereTo),
          between=gender
     )
     
     statsAll=ezStats(
          data=subset,
          dv=UhlovaVzdalenostCileAbs,
          wid=newid,
          within=c(ExpFaktor,whereTo),
          within_full=c(ExpFaktor,whereTo)
     )
     
     plot.all=ggplot(statsAll,aes(x=whereTo,y=Mean,group=interaction(ExpFaktor)))+geom_bar(aes(fill=ExpFaktor),stat="identity", position=dodge)+geom_errorbar(aes(ymax=Mean+FLSD/2,ymin=Mean-FLSD/2),position=dodge,width=0.3,size=1) +labs(y="Mean angle error",x="Target reference frame")  
     
     plot.gender=ggplot(statsGender,aes(x=gender,y=Mean,split=whereTo,fill=factor(whereTo)))+geom_bar(stat="identity",position="dodge",colour="black")+geom_errorbar(aes(ymax=Mean+FLSD/2,ymin=Mean-FLSD/2),position=position_dodge(width=0.9),width=0.3) +labs(y="Mean angle error",x="Gender")+theme(legend.position="bottom") + guides(fill=guide_legend(title="Condition"))
     
     model.all=ezANOVA(
          data=subset,
          dv=UhlovaVzdalenostCileAbs,
          wid=newid,
          within=c(whereTo,ExpFaktor),
          within_full=c(whereTo,ExpFaktor),
          return_aov=T,
          detailed=T
     )
     
     ttesttable<-subset[,.(reactionTime=mean(reactionTime,na.rm=T)),by=.(newid,ExpFaktor)]
     postHoc<-pairwise.t.test(ttesttable$reactionTime,ttesttable$ExpFaktor,pool.sd=F,paired=T,p.adjust.method="bonferroni")
     
     return.list=list("stats"=stats,"plot.all"=plot.all,"model.all"=model.all,"postHoc"=postHoc,"subset"=subset)
     return(return.list)
}

doTwoWayAnova<- function(subset){
     stats=ezStats(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=c(ExpFaktor,whereTo),
          within_full=c(ExpFaktor,whereTo)
     )
     plot.all=ezPlot(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=c(ExpFaktor,whereTo),
          within_full=c(ExpFaktor,whereTo),
          split=whereTo,
          x=ExpFaktor,
          bar_width=0.1,
          y_lab="Mean(RT)",
          x_lab="Condition",
          split_lab="Target reference frame"
     )
     model.all=ezANOVA(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=c(ExpFaktor,whereTo),
          within_full=c(ExpFaktor,whereTo),
          return_aov=T,
          detailed=T
     )
     
     
     return.list=list("stats"=stats,"plot.all"=plot.all,"model.all"=model.all,"subset"=subset)
     return(return.list)
}

doBetweenPhasesUhel <- function(subset){
     statsAll=ezStats(
          data=subset,
          dv=UhlovaVzdalenostCileAbs,
          wid=newid,
          within=c(whereTo,ExpFaktor),
          within_full=c(whereTo,ExpFaktor),
          between = test.phase
     )
     statsGender=ezStats(
          data=subset,
          dv=UhlovaVzdalenostCileAbs,
          wid=newid,
          within=c(whereTo,ExpFaktor),
          between=c(test.phase,gender)
     )
     plot.all=ggplot(statsAll,aes(x=whereTo,y=Mean,group=interaction(test.phase,ExpFaktor)))+geom_bar(aes(fill=ExpFaktor),stat="identity", position=dodge)+geom_errorbar(aes(ymax=Mean+FLSD/2,ymin=Mean-FLSD/2),position=dodge,width=0.3,size=1) +labs(y="Mean angle error",x="Target reference frame") + facet_grid(.~test.phase)      
     return.list=list("stats.all"=statsAll,"plot.all"=plot.all)
     return(return.list)
}
# --------
# --------
doAngleGoalTime <- function(subset){

     plot.exp = ggplot(subset,aes(x=abs(distance1),y=reactionTime))+geom_point(aes(colour = ExpFaktor)) + geom_smooth(aes(group=ExpFaktor, colour = ExpFaktor), method="lm", fullrange=T)  
     
     plot.where = ggplot(subset,aes(x=abs(distance1),y=reactionTime))+geom_point(aes(colour = whereTo))+ geom_smooth(aes(group=whereTo, colour = whereTo), method="lm", fullrange=T)
     
     plot.both = ggplot(subset,aes(x=abs(distance1),y=reactionTime))+geom_point(aes(colour = ExpFaktor))+ geom_smooth(aes(group=ExpFaktor, colour = ExpFaktor), method="lm", fullrange=F) + facet_wrap(~whereTo)
     
     return.list=list("plot.exp"=plot.exp,"plot.where"=plot.where,"plot.both" = plot.both,"subset" = subset)
     return(return.list)
}

# ----------------------------
#    Angular errors
# ----------------------------
subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="partial.alternation",]
Uhel.F3.1.F<-list(doAnovaThingsUhel(subset))[[1]]
TwoWay.F3.1.F<-list(doTwoWayAnova(subset))[[1]]
subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="partial.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")),]
Uhel.F4.1.F<-list(doAnovaThingsUhel(subset))[[1]]
TwoWay.F4.1.F<-list(doTwoWayAnova(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="partial.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")),]
Uhel.F5.1.F<-list(doAnovaThingsUhel(subset))[[1]]
TwoWay.F5.1.F<-list(doTwoWayAnova(subset))[[1]]
subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="full.alternation",]
Uhel.F3.1.T<-list(doAnovaThingsUhel(subset))[[1]]
TwoWay.F3.1.T<-list(doTwoWayAnova(subset))[[1]]
subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="full.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")),]
Uhel.F4.1.T<-list(doAnovaThingsUhel(subset))[[1]]
TwoWay.F4.1.T<-list(doTwoWayAnova(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="full.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")),]
Uhel.F5.1.T<-list(doAnovaThingsUhel(subset))[[1]]
TwoWay.F5.1.T<-list(doTwoWayAnova(subset))[[1]]

subset=useTableCor[(exp.version==1 & same.letters=="partial.alternation"),]
Uhel.All.1.F <- list(doAnovaThingsUhel(subset))[[1]]
Uhel.All.1.F.Graphs<- list(doBetweenPhasesUhel(subset))[[1]]
TwoWay.All.1.F<- list(doTwoWayAnova(subset))[[1]]

subset=useTableCor[test.phase %in% c('F4','F5') & exp.version==1 & same.letters=="full.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")),]
Uhel.F45.1.T <-list(doAnovaThingsUhel(subset))[[1]]
Uhel.F45.1.T.Graphs<- list(doBetweenPhasesUhel(subset))[[1]]
TwoWay.F45.1.T<- list(doTwoWayAnova(subset))[[1]]
# ----------------------------
#    Speed to goal
# ----------------------------
subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="partial.alternation",]
TimeAngle.F3.1.F <-list(doAngleGoalTime(subset))[[1]]


subset=useTableCor[exp.version==1 & same.letters=="partial.alternation",]
TimeAngle.All.1.F <-list(doAngleGoalTime(subset))[[1]]
Uhel.All.1.F <- list(doAnovaThingsUhel(subset))[[1]]

subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="full.alternation",]
TimeAngle.F3.1.T <-list(doAngleGoalTime(subset))[[1]]
subset=useTableCor[test.phase!='F3' & exp.version==1 & same.letters=="full.alternation",]
TimeAngle.F45.1.T <-list(doAngleGoalTime(subset))[[1]]

subset=useTableCor[exp.version==1 & same.letters=="partial.alternation",]
Uhel.All.1.F <- list(doBetweenPhasesUhel(subset))[[1]]

# ----------------------------
#    Two way anova
# ----------------------------
subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="partial.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")),]
MIDresultsF4.1.F<-list(doAnovaThingsUhel(subset))[[1]]


