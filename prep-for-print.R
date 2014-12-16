#Prepare all
setwd("U:/Vyzkum/Diplomka/DiplomkaGit")
library(ggplot2)
library(data.table)
library(plyr)
library(scales)
library(ez)
library(car)
library(tables)
library(reshape)
library(psych)
library(xtable)

source("output-helpers.R")

# newTable1=fread("newTableVer1.txt",sep=";",header=T,autostart=1)
# newTable2=fread("newTableVer2.txt",sep=";",header=T,autostart=1)
# newTable=rbind(newTable1,newTable2)
# newTable[,newid:=paste(id,".",exp.version,sep="",collapse=""),by=.(id,exp.version,test.phase,Faze)]

#newTable=fread("newTableVerAll.txt",sep=";",header=T,autostart=1)
newTable=fread("newTableVerAll3.0.txt",sep=";",header=T,autostart=1)
#remove one group - too small
newTable=newTable[!(same.letters==F & exp.version==2)]
newTable[,newid:=factor(newid)]
newTable[,same.letters:=factor(same.letters,levels=c(T,F),labels=c("full.alternation","partial.alternation"))]
newTable[,ExpFaktor:=factor(kamilFaktor,levels=c("same.goal","dif.goal","dif.reference","NA"),labels=c("same.goal","different.goal","different.reference.frame","NA"))]
newTable[,kamilFaktor:=NULL]
newTable[,exp.version:=factor(exp.version)]
setkey(newTable,newid,exp.version,test.phase,Faze)
newTable[,rtCorrected:=reactionTime-meanLetterRT]

#setting ggplot
theme_set(theme_bw(base_size = 25))
update_geom_defaults("line",   list(size= 1))

#nice demographic table
dem.table=tabular((Experiment=factor(exp.version)+1)*(Sequence=factor(same.letters)+1)~(Gender=factor(gender))*((n=1)+age*(mean+sd+min+max)),unique(newTable[,.(age,exp.version,same.letters,gender,newid)]))
#nice demographic table
dem.table1=tabular((Experiment=factor(exp.version))*(Sequence=factor(same.letters)+1)~(Gender=factor(gender))*((n=1)+age*(mean+sd+min+max)),unique(newTable[exp.version==1,.(age,exp.version,same.letters,gender,newid)]))
#nice demographic table
dem.table2=tabular((Experiment=factor(exp.version))*(Sequence=factor(same.letters)+1)~(Gender=factor(gender))*((n=1)+age*(mean+sd+min+max)),unique(newTable[exp.version==2,.(age,exp.version,same.letters,gender,newid)]))

t=unique(data.frame(newTable[same.letters=="partial.alternation" & exp.version==1,list(gender,correct.ans,id,same.letters,correctAnsQuant,AnsQuant,exp.version,test.phase)]))
index<-with(t,order(test.phase,correctAnsQuant,id,correct.ans))
t=t[index,]
t$id<-reorder(t$id,t$correctAnsQuant)
corrplot.1.partial=ggplot(t,aes(x=id,fill=interaction(correct.ans),y = AnsQuant))+geom_bar(stat='identity',position='fill')+facet_wrap(.(test.phase),scales='free')+ geom_hline(aes(yintercept = 0.75)) + ylab("Ratio of answers")+xlab("Individual performances")+theme(axis.ticks = element_blank(), text = element_text(size=15), axis.text.x = element_blank(),legend.position="bottom") + guides(fill=guide_legend(title="Answer type"))

t=unique(data.frame(newTable[same.letters=="full.alternation" & exp.version==1,list(gender,correct.ans,id,same.letters,correctAnsQuant,AnsQuant,exp.version,test.phase)]))
index<-with(t,order(test.phase,correctAnsQuant,id,correct.ans))
t=t[index,]
t$id<-reorder(t$id,t$correctAnsQuant)
corrplot.1.full=ggplot(t,aes(x=id,fill=interaction(correct.ans),y = AnsQuant))+geom_bar(stat='identity',position='fill')+facet_wrap(.(test.phase),scales='free')+ geom_hline(aes(yintercept = 0.75))+ ylab("Ratio of answers")+xlab("Individual performances")+theme(axis.ticks = element_blank(), text = element_text(size=15), axis.text.x = element_blank(),legend.position="bottom") + guides(fill=guide_legend(title="Answer type"))

t=unique(data.frame(newTable[same.letters=="full.alternation" & exp.version==2,list(gender,correct.ans,id,same.letters,correctAnsQuant,AnsQuant,exp.version,test.phase)]))
index<-with(t,order(test.phase,correctAnsQuant,id,correct.ans))
t=t[index,]
t$id<-reorder(t$id,t$correctAnsQuant)
corrplot.2.full=ggplot(t,aes(x=id,fill=interaction(correct.ans),y = AnsQuant))+geom_bar(stat='identity',position='fill')+facet_wrap(.(test.phase),scales='free')+ geom_hline(aes(yintercept = 0.75))+ ylab("Ratio of answers")+xlab("Individual performances")+theme(axis.ticks = element_blank(),text = element_text(size=15), axis.text.x = element_blank(),legend.position="bottom") + guides(fill=guide_legend(title="Answer type"))

setorder(newTable,newid,test.phase,Faze)
newTable[,percentAns:=.SD$AnsQuant/nrow(.SD),by=list(newid,test.phase)]

useTable = newTable[correct.ans=='CORRECT' & changedGoal!="NA" & reactionTime<5]
smallFrame=unique(newTable[,list(newid,test.phase)])
#nrow(unique(newTable[(correct.ans=='INCORRECT' & percentAns>0.1), list(newid,test.phase)]))
#list of ids to discard from the analysis
#wrong.ones=unique(newTable[correct.ans=='INCORRECT' & percentAns>0.1, list(newid,test.phase)])
#smallFrame=rbind(smallFrame,wrong.ones)
#correct.ones=smallFrame[!(duplicated(smallFrame) | duplicated(smallFrame, fromLast = TRUE)), ]
correct.ones=unique(newTable[correct.ans=='CORRECT' & percentAns>0.75, list(newid,test.phase)])
nrow(unique(newTable[correct.ans=='CORRECT' & percentAns<0.75, list(newid,test.phase)]))
useTableCor=merge(useTable,correct.ones,by=c("newid","test.phase"))

doAnovaThings<-function(subset){
     stats=ezStats(
          data=subset[rtCorrected>0],
          dv=rtCorrected,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=gender,
     )
     
     statsGender=ezStats(
          data=subset[rtCorrected>0],
          dv=rtCorrected,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=gender,
     )
     
     statsAll=ezStats(
          data=subset[rtCorrected>0],
          dv=rtCorrected,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
     )
     
     if(subset$same.letters[1]=='full.alternation' & subset$test.phase != 'F3'){
          typeNum=12
     } else {
          typeNum=9
     }
     descr.table=tabular(((Gender=factor(gender))+Hline(2:typeNum)+1)~Format(digits=3)*Heading()*rtCorrected*(Condition=factor(ExpFaktor))*(mean+sd+min+max),subset[rtCorrected>0])
     
     plot.all=ggplot(statsAll,aes(x=ExpFaktor,y=Mean,split=ExpFaktor))+geom_bar(stat="identity",fill="grey",colour="black")+geom_errorbar(aes(ymax=Mean+FLSD/2,ymin=Mean-FLSD/2),width=0.3,size=1) +labs(y="Mean reaction time (s)",x="Condition") +  scale_y_continuous(limits=c(0,3))
     
     plot.gender=ggplot(statsGender,aes(x=gender,y=Mean,split=ExpFaktor,fill=factor(ExpFaktor)))+geom_bar(stat="identity",position="dodge",colour="black")+geom_errorbar(aes(ymax=Mean+FLSD/2,ymin=Mean-FLSD/2),position=position_dodge(width=0.9),width=0.3) +labs(y="Mean reaction time (s)",x="Gender")+  scale_y_continuous(limits=c(0,3))+theme(legend.position="bottom") + guides(fill=guide_legend(title="Condition"))
     
     plot.gender2=ezPlot(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=gender,
          split=gender,
          x=ExpFaktor,
          bar_width=0.1,
          y_lab="Mean(RT)",
          x_lab="Condition",
          split_lab="Gender"
     )
     
     plot.all2=ezPlot(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          x=ExpFaktor,
          bar_width=0.1,
          y_lab="Mean(RT)",
          x_lab="Condition",
     )
     
    
     model.gender=ezANOVA(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=gender,
          detailed=T,
     )
     model.all=ezANOVA(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          return_aov=T,
          detailed=T,
     )
     plot.all.rtCorr=ezPlot(
          data=subset[rtCorrected>0],
          dv=rtCorrected,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          x=ExpFaktor,
          bar_width=0.1,
          y_lab="Mean(Corrected RT)",
          x_lab="Condition",
     )
     
     model.all.rtCorr=ezANOVA(
          data=subset[rtCorrected>0],
          dv=rtCorrected,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          return_aov=T,
          detailed=T,
     )
     
     #creates a nice post.hoc table
     #ttesttable<-subset[,.(reactionTime=mean(reactionTime,na.rm=T)),by=.(newid,ExpFaktor)]
     ttesttable<-subset[rtCorrected>0,.(reactionTime=mean(rtCorrected,na.rm=T)),by=.(newid,ExpFaktor)]
     postHoc<-pairwise.t.test(ttesttable$reactionTime,ttesttable$ExpFaktor,pool.sd=F,paired=T,p.adjust.method="bonferroni")
     postHocTable<-apply(postHoc$p.value,1:2,apaPtable)

     plot.singular=ggplot(subset[,list(mean=mean(reactionTime)),by=list(newid,ExpFaktor)],aes(y=mean,x=ExpFaktor,group=newid,colour=newid))
     plot.singular=plot.singular + geom_point() + geom_line()
     return.list=list("stats"=stats,"plot.all"=plot.all,"plot.gender"=plot.gender,"model.all"=model.all,"model.gender"=model.gender,"plot.singular"=plot.singular, "descr.table"=descr.table,"model.all.rtCorr"=model.all.rtCorr,"plot.all.rtCorr"=plot.all.rtCorr,"postHoc"=postHoc,"subset"=subset,"postHocTable"=postHocTable)
     return(return.list)
}

doAnovaAcross<-function(subset){
     if(subset$same.letters[1]=='full.alternation' & subset$test.phase != 'F3'){
          typeNum=12
     } else {
          typeNum=9
     }
     
     descr.table=tabular(((Version=factor(exp.version))+Hline(2:typeNum))~Format(digits=3)*Heading()*reactionTime*(Condition=factor(ExpFaktor))*(mean+sd+min+max),subset)
     
     statsAll=ezStats(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=exp.version,
          between_full=exp.version
     )

     plot.all=ggplot(statsAll,aes(x=exp.version,y=Mean,split=ExpFaktor,fill=factor(ExpFaktor)))+geom_bar(stat="identity",position="dodge",colour="black")+geom_errorbar(aes(ymax=Mean+FLSD/2,ymin=Mean-FLSD/2),position=position_dodge(width=0.9),width=0.3) + labs(y="Mean reaction time (s)",x="Experiment version") +  scale_y_continuous(limits=c(0,3))+theme(legend.position="bottom") + guides(fill=guide_legend(title="Condition"))
     
     plot.all2=ezPlot(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=exp.version,
          split=exp.version,
          x=ExpFaktor,
          bar_width=0.1,
          y_lab="Mean(RT)",
          x_lab="Condition",
          split_lab="Experiment"
     )
     
     model.all=ezANOVA(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=exp.version,
          return_aov=T,
          detailed=T,
          type=3
     )    
     model.all.rtCorr=ezANOVA(
          data=subset[rtCorrected>0],
          dv=rtCorrected,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=exp.version,
          return_aov=T,
          detailed=T,
          type=3
     )
     return.list=list("plot.all"=plot.all,"model.all"=model.all,"descr.table"=descr.table,"subset"=subset,"model.all.rtCorr"=model.all.rtCorr)
     return(return.list)
}

doAnovaThingsCovariates<-function(subset){
     
stats=ezStats(
data=subset,
dv=reactionTime,
wid=newid,
within=ExpFaktor,
within_full=ExpFaktor,
within_covariates=distance1,
between=gender,
)
plot.gender=ezPlot(
data=subset,
dv=reactionTime,
wid=newid,
within=ExpFaktor,
within_full=ExpFaktor,
within_covariates=distance1,
between=gender,
split=gender,
x=ExpFaktor,
)
plot.all=ezPlot(
data=subset,
dv=reactionTime,
wid=newid,
within=ExpFaktor,
within_full=ExpFaktor,
within_covariates=distance1,
x=ExpFaktor,
)
model.gender=ezANOVA(
data=subset,
dv=reactionTime,
wid=newid,
within=ExpFaktor,
within_full=ExpFaktor,
within_covariates=distance1,
between=gender,
detailed=T,
return_aov=T
)
model.all=ezANOVA(
data=subset,
dv=reactionTime,
wid=newid,
within=ExpFaktor,
within_full=ExpFaktor,
within_covariates=distance1,
detailed=T,
return_aov=T
)
plot.singular=ggplot(subset[,list(mean=mean(reactionTime)),by=list(newid,ExpFaktor)],aes(y=mean,x=ExpFaktor,group=newid,colour=newid))
plot.singular=plot.singular + geom_point() + geom_line()
return.list=list("stats"=stats,"plot.all"=plot.all,"plot.gender"=plot.gender,"model.all"=model.all,"model.gender"=model.gender,"plot.singular"=plot.singular)
return(return.list)
}


###
#FIRST EXPERIMENT
#PARTIAL ALTERNATION
###

subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="partial.alternation",]
resultsF3.1.F<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="partial.alternation",]
resultsF4.1.F<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="partial.alternation",]
resultsF5.1.F<-list(doAnovaThings(subset))[[1]]

###
#FIRST EXPERIMENT
#PARTIAL ALTERNATION
#ONLY MIDDLE
###
subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="partial.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")),]
MIDresultsF4.1.F<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="partial.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")),]
MIDresultsF5.1.F<-list(doAnovaThings(subset))[[1]]

###
#FIRST EXPERIMENT
#PARTIAL ALTERNATION
#DIFFERENTIATING ARENA AND AI
###

subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="partial.alternation" & whereTo=='Arena',]
resultsF3.1.F.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="partial.alternation" & whereTo=='AI',]
resultsF3.1.F.F<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="partial.alternation" & whereTo=='Arena',]
resultsF4.1.F.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="partial.alternation" & whereTo=='AI',]
resultsF4.1.F.F<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="partial.alternation" & whereTo=='Arena',]
resultsF5.1.F.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="partial.alternation" & whereTo=='AI',]
resultsF5.1.F.F<-list(doAnovaThings(subset))[[1]]

###
#FIRST EXPERIMENT
#PARIAL ALTERNATION
#DIFFERENTIATING ARENA AND AI
#ONLY MIDDLE
###

subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="partial.alternation" & JmenoOrientacnihoBodu == "Mezi-cily" & whereTo=='Arena',]
MIDresultsF4.1.F.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="partial.alternation" & JmenoOrientacnihoBodu == "Mezi-cily" & whereTo=='Arena',]
MIDresultsF5.1.F.A<-list(doAnovaThings(subset))[[1]]

###
#FIRST EXPERIMENT
#FULL ALTERNATION
###

subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="full.alternation",]
resultsF3.1.T<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="full.alternation",]
resultsF4.1.T<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="full.alternation",]
resultsF5.1.T<-list(doAnovaThings(subset))[[1]]

###
#FIRST EXPERIMENT
#FULL ALTERNATION
#ONLY MIDDLE
###
subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="full.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")),]
MIDresultsF4.1.T<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="full.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")),]
MIDresultsF5.1.T<-list(doAnovaThings(subset))[[1]]
###
#FIRST EXPERIMENT
#FULL ALTERNATION
#DIFFERENTIATING ARENA AND AI
###

subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="full.alternation" & whereTo=='Arena',]
resultsF3.1.T.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="full.alternation" & whereTo=='AI',]
resultsF3.1.T.F<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="full.alternation" & whereTo=='Arena',]
resultsF4.1.T.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="full.alternation" & whereTo=='AI',]
resultsF4.1.T.F<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="full.alternation" & whereTo=='Arena',]
resultsF5.1.T.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="full.alternation" & whereTo=='AI',]
resultsF5.1.T.F<-list(doAnovaThings(subset))[[1]]

###
#FIRST EXPERIMENT
#FULL ALTERNATION
#DIFFERENTIATING ARENA AND AI
#ONLY MIDDLE - the figure is effectively the same
###

subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="full.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo")) & whereTo=='Arena',]
MIDresultsF4.1.T.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="full.alternation" & JmenoOrientacnihoBodu == "Mezi-cily" & whereTo=='Arena',]
MIDresultsF5.1.T.A<-list(doAnovaThings(subset))[[1]]


###
#SECOND EXPERIMENT
###

subset=useTableCor[test.phase=='F3' & exp.version==2 & same.letters=="full.alternation",]
resultsF3.3.T<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F4' & exp.version==2 & same.letters=="full.alternation",]
resultsF4.3.T<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==2 & same.letters=="full.alternation",]
resultsF5.3.T<-list(doAnovaThings(subset))[[1]]

###
#SECOND EXPERIMENT
#MIDDLE
###

subset=useTableCor[test.phase=='F4' & exp.version==2 & same.letters=="full.alternation" & JmenoOrientacnihoBodu %in% c("Mezi-cily","AI Character"),]
MIDresultsF4.3.T<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==2 & same.letters=="full.alternation" & JmenoOrientacnihoBodu %in% c("Mezi-cily","AI Character"),]
MIDresultsF5.3.T<-list(doAnovaThings(subset))[[1]]

###
#SECOND EXPERIMENT
#DIFFERENTIATING ARENA AND AI
###

subset=useTableCor[test.phase=='F3' & exp.version==2 & same.letters=="full.alternation" & whereTo=='Arena',]
resultsF3.3.T.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F3' & exp.version==2 & same.letters=="full.alternation" & whereTo=='AI',]
resultsF3.3.T.F<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F4' & exp.version==2 & same.letters=="full.alternation" & whereTo=='Arena',]
resultsF4.3.T.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F4' & exp.version==2 & same.letters=="full.alternation" & whereTo=='AI',]
resultsF4.3.T.F<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F5' & exp.version==2 & same.letters=="full.alternation" & whereTo=='Arena',]
resultsF5.3.T.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==2 & same.letters=="full.alternation" & whereTo=='AI',]
resultsF5.3.T.F<-list(doAnovaThings(subset))[[1]]

###
#SECOND EXPERIMENT
#DIFFERENTIATING ARENA AND AI
#ONLY MIDDLE
###

subset=useTableCor[test.phase=='F4' & exp.version==2 & same.letters=="full.alternation" & whereTo=='Arena' & JmenoOrientacnihoBodu=="Mezi-cily",]
MIDresultsF4.3.T.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F4' & exp.version==2 & same.letters=="full.alternation" & whereTo=='AI' & JmenoOrientacnihoBodu=="AI Character",]
MIDresultsF4.3.T.F<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F5' & exp.version==2 & same.letters=="full.alternation" & whereTo=='Arena' & JmenoOrientacnihoBodu=="Mezi-cily",]
MIDresultsF5.3.T.A<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==2 & same.letters=="full.alternation" & whereTo=='AI' & JmenoOrientacnihoBodu=="AI Character",]
MIDresultsF5.3.T.F<-list(doAnovaThings(subset))[[1]]

###
#BETWEEN GROUP RESULTS
###
subset=useTableCor[test.phase=='F3' & same.letters=="full.alternation" & whereTo=='Arena',]
resultsF3.B.A<-doAnovaAcross(subset)
subset=useTableCor[test.phase=='F3' & same.letters=="full.alternation" & whereTo=='AI',]
resultsF3.B.F<-doAnovaAcross(subset)

subset=useTableCor[test.phase=='F4' & same.letters=="full.alternation" & whereTo=='Arena',]
resultsF4.B.A<-doAnovaAcross(subset)
subset=useTableCor[test.phase=='F4' & same.letters=="full.alternation" & whereTo=='AI',]
resultsF4.B.F<-doAnovaAcross(subset)

subset=useTableCor[test.phase=='F5' & same.letters=="full.alternation" & whereTo=='Arena',]
resultsF5.B.A<-doAnovaAcross(subset)
subset=useTableCor[test.phase=='F5' & same.letters=="full.alternation" & whereTo=='AI',]
resultsF5.B.F<-doAnovaAcross(subset)

###
#BETWEEN GROUP RESULTS
###
subset=useTableCor[test.phase=='F4' & same.letters=="full.alternation" & whereTo=='Arena' & JmenoOrientacnihoBodu %in% c("AI Character","Mezi-cily"),]
MIDresultsF4.B.A<-doAnovaAcross(subset)

subset=rbind(MIDresultsF4.3.T.F$subset,resultsF4.1.T.F$subset)
MIDresultsF4.B.F<-doAnovaAcross(subset)

subset=useTableCor[test.phase=='F5' & same.letters=="full.alternation" & whereTo=='Arena' & JmenoOrientacnihoBodu == "Mezi-cily",]
MIDresultsF5.B.A<-doAnovaAcross(subset)

subset=rbind(MIDresultsF5.3.T.F$subset,resultsF5.1.T.F$subset)
MIDresultsF5.B.F<-doAnovaAcross(subset)
