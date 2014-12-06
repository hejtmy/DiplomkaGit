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

apaP<-function(value){
     if(is.na(value)){
          return(NA)
     }
     if (value < 0.001){
          return("p \\textless .001 ")
     }
     if (value < 0.01){
          return("p \\textless .01 ")
     }
     if (value < 0.03){
          return("p \\textless .03 ")
     }
     if (value < 0.05){
          return("p \\textless .05 ")
     }
     if (value < 0.1){
          return(paste("p = ",gsub("^(-?)0.", "\\1.", round(value,digits=3)),sep=""))
     }
     if (value >0.1){
          return(paste("p = ",gsub("^(-?)0.", "\\1.", format(value,digits=3)),sep=""))
     }
}
apaCor<-function(value){
     return(gsub("^(-?)0.", "\\1.", format(value,digits=3)))
}
apaPtable<-function(value){
     if(is.na(value)){
          return(NA)
     }
     if (value < 0.001){
          return("\\textless .001 ")
     }
     if (value < 0.01){
          return("\\textless .01 ")
     }
     if (value < 0.03){
          return("\\textless .03 ")
     }
     if (value < 0.05){
          return("\\textless .05 ")
     }
     if (value < 0.1){
          return(paste(gsub("^(-?)0.", "\\1.", round(value,digits=3)),sep=""))
     }
     if (value >0.1){
          return(paste(gsub("^(-?)0.", "\\1.", format(value,digits=3)),sep=""))
     }
     
}

# newTable1=fread("newTableVer1.txt",sep=";",header=T,autostart=1)
# newTable2=fread("newTableVer2.txt",sep=";",header=T,autostart=1)
# newTable=rbind(newTable1,newTable2)
# newTable[,newid:=paste(id,".",exp.version,sep="",collapse=""),by=.(id,exp.version,test.phase,Faze)]

#newTable=fread("newTableVerAll.txt",sep=";",header=T,autostart=1)
newTable=fread("newTableVerAll3.0.txt",sep=";",header=T,autostart=1)
#remove one group - too small
newTable=newTable[!(same.letters==F & exp.version==2)]
newTable[,newid:=factor(newid)]
newTable
newTable[,same.letters:=factor(same.letters,levels=c(T,F),labels=c("full.alternation","partial.alternation"))]
newTable[,ExpFaktor:=factor(kamilFaktor,levels=c("same.goal","dif.goal","dif.reference","NA"))]
newTable[,kamilFaktor:=NULL]
newTable[,exp.version:=factor(exp.version)]
setkey(newTable,newid,exp.version,test.phase,Faze)
newTable[,rtCorrected:=reactionTime-meanLetterRT]

#nice demographic table
dem.table=tabular((Experiment=factor(exp.version)+1)*(Sequence=factor(same.letters)+1)~(Gender=factor(gender))*((n=1)+age*(mean+sd+min+max)),unique(newTable[,.(age,exp.version,same.letters,gender,newid)]))
#nice demographic table
dem.table1=tabular((Experiment=factor(exp.version))*(Sequence=factor(same.letters)+1)~(Gender=factor(gender))*((n=1)+age*(mean+sd+min+max)),unique(newTable[exp.version==1,.(age,exp.version,same.letters,gender,newid)]))
#nice demographic table
dem.table2=tabular((Experiment=factor(exp.version))*(Sequence=factor(same.letters)+1)~(Gender=factor(gender))*((n=1)+age*(mean+sd+min+max)),unique(newTable[exp.version==2,.(age,exp.version,same.letters,gender,newid)]))

t=unique(data.frame(newTable[test.phase=='F3',list(gender,correct.ans,id,same.letters,correctAnsQuant,AnsQuant,exp.version,newid)]))
index<-with(t,order(exp.version,same.letters,correctAnsQuant,id,correct.ans))
t=t[index,]
t$newid<-reorder(t$newid,t$correctAnsQuant)
corrplotF3=ggplot(t,aes(x=newid,fill=interaction(gender,correct.ans),y = AnsQuant))+geom_bar(stat='identity',position='fill')+facet_wrap(.(same.letters,exp.version),scales='free')+ geom_hline(aes(yintercept = 0.75))

t=unique(data.frame(newTable[same.letters=="partial.alternation" & exp.version==1,list(gender,correct.ans,id,same.letters,correctAnsQuant,AnsQuant,exp.version,test.phase)]))
index<-with(t,order(test.phase,correctAnsQuant,id,correct.ans))
t=t[index,]
t$id<-reorder(t$id,t$correctAnsQuant)
corrplot.1.partial=ggplot(t,aes(x=id,fill=interaction(gender,correct.ans),y = AnsQuant))+geom_bar(stat='identity',position='fill')+facet_wrap(.(test.phase),scales='free')+ geom_hline(aes(yintercept = 0.75))

t=unique(data.frame(newTable[same.letters=="partial.alternation" & exp.version==2,list(gender,correct.ans,id,same.letters,correctAnsQuant,AnsQuant,exp.version,test.phase)]))
index<-with(t,order(test.phase,correctAnsQuant,id,correct.ans))
t=t[index,]
t$id<-reorder(t$id,t$correctAnsQuant)
corrplot.2.partial=ggplot(t,aes(x=id,fill=interaction(gender,correct.ans),y = AnsQuant))+geom_bar(stat='identity',position='fill')+facet_wrap(.(test.phase),scales='free')+ geom_hline(aes(yintercept = 0.75))

t=unique(data.frame(newTable[same.letters=="full.alternation" & exp.version==1,list(gender,correct.ans,id,same.letters,correctAnsQuant,AnsQuant,exp.version,test.phase)]))
index<-with(t,order(test.phase,correctAnsQuant,id,correct.ans))
t=t[index,]
t$id<-reorder(t$id,t$correctAnsQuant)
corrplot.1.full=ggplot(t,aes(x=id,fill=interaction(gender,correct.ans),y = AnsQuant))+geom_bar(stat='identity',position='fill')+facet_wrap(.(test.phase),scales='free')+ geom_hline(aes(yintercept = 0.75))

t=unique(data.frame(newTable[same.letters=="full.alternation" & exp.version==2,list(gender,correct.ans,id,same.letters,correctAnsQuant,AnsQuant,exp.version,test.phase)]))
index<-with(t,order(test.phase,correctAnsQuant,id,correct.ans))
t=t[index,]
t$id<-reorder(t$id,t$correctAnsQuant)
corrplot.2.full=ggplot(t,aes(x=id,fill=interaction(gender,correct.ans),y = AnsQuant))+geom_bar(stat='identity',position='fill')+facet_wrap(.(test.phase),scales='free')+ geom_hline(aes(yintercept = 0.75))

t=unique(data.frame(newTable[test.phase=='F4',list(gender,correct.ans,id,same.letters,correctAnsQuant,AnsQuant,exp.version,newid)]))
index<-with(t,order(exp.version,same.letters,correctAnsQuant,id,correct.ans))
t=t[index,]
t$newid<-reorder(t$newid,t$correctAnsQuant)
corrplotF4=ggplot(t,aes(x=newid,fill=interaction(gender,correct.ans),y = AnsQuant))+geom_bar(stat='identity',position='fill')+facet_wrap(.(same.letters,exp.version),scales='free')+ geom_hline(aes(yintercept = 0.75))

t=unique(data.frame(newTable[test.phase=='F5',list(gender,correct.ans,id,same.letters,correctAnsQuant,AnsQuant,exp.version,newid)]))
index<-with(t,order(exp.version,same.letters,correctAnsQuant,id,correct.ans))
t=t[index,]
t$newid<-reorder(t$newid,t$correctAnsQuant)
corrplotF5=ggplot(t,aes(x=newid,fill=interaction(gender,correct.ans),y = AnsQuant))+geom_bar(stat='identity',position='fill')+facet_wrap(.(same.letters,exp.version),scales='free')+ geom_hline(aes(yintercept = 0.75))

setorder(newTable,newid,test.phase,Faze)
newTable[,percentAns:=.SD$AnsQuant/nrow(.SD),by=list(newid,test.phase)]

useTable = newTable[correct.ans=='CORRECT' & changedGoal!="NA" & reactionTime<5]
smallFrame=unique(newTable[,list(newid,test.phase)])
#nrow(unique(newTable[(correct.ans=='INCORRECT' & percentAns>0.1), list(newid,test.phase)]))
#list of ids to discard from the analysis
wrong.ones=unique(newTable[correct.ans=='INCORRECT' & percentAns>0.1, list(newid,test.phase)])
smallFrame=rbind(smallFrame,wrong.ones)
correct.ones=smallFrame[!(duplicated(smallFrame) | duplicated(smallFrame, fromLast = TRUE)), ]
correct.ones=unique(newTable[correct.ans=='CORRECT' & percentAns>0.75, list(newid,test.phase)])
nrow(unique(newTable[correct.ans=='CORRECT' & percentAns<0.75, list(newid,test.phase)]))
useTableCor=merge(useTable,correct.ones,by=c("newid","test.phase"))

doAnovaThings<-function(subset){
     stats=ezStats(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=gender,
     )
     if(subset$same.letters[1]=='full.alternation' & subset$test.phase != 'F3'){
          typeNum=12
     } else {
          typeNum=9
     }
     descr.table=tabular(((Gender=factor(gender))+Hline(2:typeNum)+1)~Format(digits=3)*Heading()*reactionTime*(Condition=factor(ExpFaktor))*(mean+sd+min+max),subset)
     
     plot.gender=ezPlot(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=gender,
          split=gender,
          x=ExpFaktor,
          bar_width=0.1,
     )
     
     plot.all=ezPlot(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          x=ExpFaktor,
          bar_width=0.1,
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
     ttesttable<-subset[,.(reactionTime=mean(reactionTime,na.rm=T)),by=.(newid,ExpFaktor)]
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
     
     
     plot.all=ezPlot(
          data=subset,
          dv=reactionTime,
          wid=newid,
          within=ExpFaktor,
          within_full=ExpFaktor,
          between=exp.version,
          split=exp.version,
          x=ExpFaktor,
          bar_width=0.1,
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
     return.list=list("plot.all"=plot.all,"model.all"=model.all,"descr.table"=descr.table,"subset"=subset)
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

subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="partial.alternation",]
resultsF3.1.F<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F3' & exp.version==1 & same.letters=="full.alternation",]
resultsF3.1.T<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="partial.alternation",]
resultsF4.1.F<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="full.alternation",]
resultsF4.1.T<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="partial.alternation",]
resultsF5.1.F<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="full.alternation",]
resultsF5.1.T<-list(doAnovaThings(subset))[[1]]


subset=useTableCor[test.phase=='F3' & exp.version==2 & same.letters=="full.alternation",]
resultsF3.3.T<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F4' & exp.version==2 & same.letters=="full.alternation",]
resultsF4.3.T<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F5' & exp.version==2 & same.letters=="full.alternation",]
resultsF5.3.T<-list(doAnovaThings(subset))[[1]]

# subset=useTableCor[test.phase=='F4' & exp.version==2 & same.letters=="partial.alternation",]
# resultsF4.3.F<-list(doAnovaThings(subset))[[1]]
# subset=useTableCor[test.phase=='F3' & exp.version==2 & same.letters=="partial.alternation",]
# resultsF3.3.F<-list(doAnovaThings(subset))[[1]]
# subset=useTableCor[test.phase=='F5' & exp.version==2 & same.letters=="partial.alternation",]
# resultsF5.3.F<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="full.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo"))& whereTo=='Arena',]
resultsF4.1.T.A.M<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="full.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo"))& whereTo=='Arena',]
resultsF5.1.T.A.M<-list(doAnovaThings(subset))[[1]]

subset=useTableCor[test.phase=='F4' & exp.version==1 & same.letters=="partial.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo"))& whereTo=='Arena',]
resultsF4.1.F.A.M<-list(doAnovaThings(subset))[[1]]
subset=useTableCor[test.phase=='F5' & exp.version==1 & same.letters=="partial.alternation" & !(JmenoOrientacnihoBodu %in% c("Cil1-vlevo","Cil2-vpravo"))& whereTo=='Arena',]
resultsF5.1.F.A.M<-list(doAnovaThings(subset))[[1]]

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

reportAnova<-function(myList){
     mauch=myList$model.all$"Mauchly's Test for Sphericity"
     spher=myList$model.all$`Sphericity Corrections`
     
     if(is.null(mauch)){
          fstat=paste("F(",myList$model.all$ANOVA$DFn[2],", ",myList$model.all$ANOVA$DFd[2],")= ",round(myList$model.all$ANOVA$F[2],digits=2), ", ",apaP(myList$model.all$ANOVA$p[2]),sep="")
          return(fstat)
     }
     
     if(mauch$p>0.05){
          fstat=paste("F(",myList$model.all$ANOVA$DFn[2],", ",myList$model.all$ANOVA$DFd[2],")= ",round(myList$model.all$ANOVA$F[2],digits=2), ", ",apaP(myList$model.all$ANOVA$p[2]),sep="")
          return(fstat)
     }    
     
     if(mauch$p<0.05){
          if(spher$GGe>0.75){
               #using Hugh-Feldt correction
              mauchly=paste("(Mauchly's test indicated that assumption of sphericity was violated, " ,apaP(mauch$p),"; Greenhouse-Geisser ($\\epsilon$ = ", round(spher$GGe,digits=2),"), degrees of freedon were corrected using Huynh-Feldt estimates of sphericity)",sep="")
              fstat=paste("F(",round(spher$HFe*myList$model.all$ANOVA$DFn[2],digits=2),", ",round(spher$HFe*myList$model.all$ANOVA$DFd[2],digits=2),") = ",round(myList$model.all$ANOVA$F[2],digits=2), ", ",apaP(spher$"p[HF]"),sep="")
              return(paste(fstat, mauchly,sep=" "))
          }
          if(spher$GGe<0.75){
               #using Hugh-Feldt correction
               mauchly=paste("(Mauchly's test indicated that assumption of sphericity was violated, " ,apaP(mauch$p),"; degrees of freedom were corrected using Greenhouse-Geisser ($\\epsilon$ = ", round(spher$GGe,digits=2),") estimates of sphericity)",sep="")
               fstat=paste("F(",round(spher$GGe*myList$model.all$ANOVA$DFn[2],digits=2),", ",round(spher$GGe*myList$model.all$ANOVA$DFd[2],digits=2),") = ",round(myList$model.all$ANOVA$F[2],digits=2), ", ",apaP(spher$"p[GG]"),sep="")
               return(paste(fstat, mauchly, sep=" "))
          } 
     }
}
reportAnovaMod<-function(myList,num=2){
          fstat=paste("F(",myList$model.all$ANOVA$DFn[num],", ",myList$model.all$ANOVA$DFd[num],")= ",round(myList$model.all$ANOVA$F[num],digits=2), ", ",apaP(myList$model.all$ANOVA$p[num]),sep="")
          return(fstat)
}
reportAnovaCor<-function(myList){
     return(paste("F(",myList$model.all.rtCorr$ANOVA$DFn[2],", ",myList$model.all.rtCorr$ANOVA$DFd[2],")=",round(myList$model.all.rtCorr$ANOVA$F[2],digits=2), ", ",apaP(myList$model.all.rtCorr$ANOVA$p[2]),sep="",collapse=" "))
}

reportRTDistanceCorrelation<-function(myList){
   return(paste("r(",length(myList$subset$reactionTime),") = ", apaCor(cor.test(myList$subset$reactionTime,myList$subset$distance1)$estimate),", ",apaP(cor.test(myList$subset$reactionTime,myList$subset$distance1)$p.value),sep=""))
}
