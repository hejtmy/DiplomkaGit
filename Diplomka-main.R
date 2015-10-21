#setwd("U:/Vyzkum/Diplomka/DiplomkaGit")
setwd("c:/users/lukáš/vyzkum/diplomka/diplomkagit")
library(ggplot2)
library(data.table)
library(plyr)
library(ez)
library(car)

#options(contrasts=c("contr.sum","contr.poly"))
dir="U:/Vyzkum/Diplomka/data/Unity/"
files = list.files(dir, full.names = T)
#loads functions
#doesnt work, has to be run manually
loadVer1<-function(){
  #directory with all the raw data files from the experiment
     dir="U:/Vyzkum/Diplomka/data/Unity/Ver1All/"
     files = list.files(dir, full.names = T, recursive=T)
     #function to for add_demographics, read_file and better_log_table
     source("Data-reader.R")
     #reads in all the LOG tables and binds them by row
     log_table = do.call("rbind",lapply(files, read_file, key="LOG", dir))
     #same for TEST results
     test_table= do.call("rbind",lapply(files, read_file, key="TEST", dir))
     test_table<-add_demographics(test_table,"Data/demographicsVer1.txt")
     log_table<-add_demographics(log_table,"Data/demographicsVer1.txt")
     
     better_log_table(log_table)
     source("Data-prep.R")
}

#doesnt work, has to be run manually
loadVer3<-function(){
     dir="U:/Vyzkum/Diplomka/data/Unity/Ver3All/"
     files = list.files(dir, full.names = T, recursive=T)
     source("Data-reader.R")
     log_table = do.call("rbind",lapply(files, read_file, key="LOG", dir))
     test_table= do.call("rbind",lapply(files, read_file, key="TEST", dir))
     test_table<-add_demographics(test_table,"Data/demographicsVer3.txt")
     log_table<-add_demographics(log_table,"Data/demographicsVer3.txt")
     better_log_table(log_table)
     source("Data-prep.R")
     return(newTable)
}

#newTable[,exp.version:=2]
write.table(newTable,"newTableVerAll3.0.txt",sep=";",row.names=F,quote=F)
###Oprava chyby

# newTable[newid=="116.1" & letter=="A",meanLetterRT:=0.2972653]
# newTable[newid=="116.1" & letter=="B",meanLetterRT:=0.2749958]
# newTable[newid=="116.1" & letter=="C",meanLetterRT:=0.3363560]
# newTable[newid=="116.1" & letter=="D",meanLetterRT:=0.2881440]

makeMyTable<-function(tab,custom.list){
     tab[,mean:=mean(reactionTime),by=custom.list]
     tab[,mean.sem:=sd(reactionTime)/sqrt(nrow(.SD)),by=custom.list]
     tab[,sd:=sd(reactionTime),by=custom.list]
     tab[,nrow:=nrow(.SD),by=custom.list]
     tab[,mean.sem.upper:=mean + 1.96*mean.sem]
     tab[,mean.sem.lower:=mean - 1.96*mean.sem]
     return(tab)
}

newTable=fread("Data/newTableVerAll3.0.txt",sep=";",header=T,autostart=1)

fit.table=makeMyTable(newTable[did.switch !="NA" & test.phase=='F3' & changedGoal!="NA"],c("id","kamilFaktor"))
fit.table=newTable[did.switch !="NA" & test.phase=='F5' & changedGoal!="NA" & same.letters=="full.allternation" & JmenoOrientacnihoBodu=="Mezi-cily"]
bar=ggplot(fit.table,aes(x=kamilFaktor,y=mean,fill=changedGoal))
bar+geom_bar(stat='identity',position='dodge') + geom_errorbar(aes(ymax=mean.sem.upper, ymin=mean.sem.lower),position=position_dodge(0.9))

spss.table=newTable[test.phase=='F5' & changedGoal!="NA" & same.letters.x==T,list(mean=mean(reactionTime)),by=list(id,kamilFaktor)]
#n=cbind(unique(spss.table[,id]),sapply(c(1,2,3), FUN=function(x) spss.table[kamilFaktor==x,mean]))
#write.table(n,"SPSSver1.txt",sep=";",row.names=F,quote=F,fileEncoding='utf8',dec=",")

summary(fit)

mod<-lm(reactionTime~kamilFaktor,fit.table)

#tested for consistency ... with within_full specified there is no issue with the interneal 
#mean calculations
model=ezANOVA(
     data=fit.table,
     dv=reactionTime,
     wid=id,
     within=kamilFaktor,
     within_full=.(kamilFaktor),
     detailed=T
)

model = ezANOVA(
    data=useTableCor[did.switch !="NA" & test.phase=='F5' & changedGoal!="NA" & same.letters==T],
    dv = reactionTime,
    wid = newid,
    within = ExpFaktor,
    between = gender,
    detailed =  T
)

sum.table=fit.table[,list(mean=mean(reactionTime)),by=list(kamilFaktor,id)]
sum.table[,kamilFaktor:=factor(kamilFaktor)]
sum.table[,id:=factor(id)]
summary(aov(mean~kamilFaktor+Error(id/kamilFaktor),sum.table))

model=ezANOVA(
     data=useTableCor[did.switch !="NA" & test.phase=='F5' & changedGoal!="NA" & same.letters==T],
     dv=reactionTime,
     wid=id,
     within=kamilFaktor,
     within_full=kamilFaktor,
     between=gender,
     detailed=T
)

plot=ezPlot(
     data=useTableCor[test.phase=='F5' & changedGoal!="NA" & same.letters==T],
     dv=reactionTime,
     wid=id,
     within=kamilFaktor,
     within_full=kamilFaktor,
     between=gender,
     split=gender,
     x=kamilFaktor
)

stats=ezStats(
     data=useTableCor[test.phase=='F4' & changedGoal!="NA" & same.letters==F],
     dv=reactionTime,
     wid=id,
     within=kamilFaktor,
     within_full=kamilFaktor,
     between=gender
)

model2=ezANOVA(
     data=fit.table[same.letters==T],
     dv=reactionTime,
     wid=id,
     within=kamilFaktor,
     detailed=T,
     type=3
)

model.gender=ezANOVA(
     data=useTableCor,
     dv=reactionTime,
     wid=newid,
     within=kamilFaktor,
     within_full=kamilFaktor,
     within_covariates=distance1,
     between=gender,
     detailed=T
)

model

hist=ggplot(newTable[did.switch !="NA" & test.phase=='F3'],aes(x=distance1))
hist+geom_histogram(binwidth=5)

scatter=ggplot(useTable[did.switch !="NA" & reactionTime<3],aes(x=distance1,y=reactionTime))
scatter+geom_point()+stat_smooth(aes(group=distance1>0),method="lm",size=1,colour="red",se=F)

summary(lm(reactionTime~distance1,newTable[did.switch !="NA" & distance1>=0]))

t=data.frame(newTable[,correct.ans,by=list(id,test.phase)])
uniq=nrow(unique(t["id"]))*3
#checks the correct answers
library(scales)
corrplot=ggplot(t,aes(x=id,fill=correct.ans,y = (..count..)*.GlobalEnv$uniq/sum(..count..)))
corrplot+geom_bar(stat='bin')+facet_wrap(~test.phase)+ scale_y_continuous(labels=percent)+ geom_hline(aes(yintercept = 0.85))

#finds people who were wrong more than often

newTable[,version:=1]
if(head(newTable)$version==1){
     newTable[JmenoOrientacnihoBodu %in% c('Ethan-vlevo','Ethan-vpravo'), JmenoOrientacnihoBodu:='AI Character']
}

bar = ggplot(useTable[did.switch != 'NA' & id %in% c(18,19,22,23,28,30,31,32,33,35,38,40,41,43,44,46,47,48,49),list(mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo)],aes(x=test.phase,y=mean,fill=c(did.switch)))
bar+geom_bar(stat='identity',position='dodge') + facet_wrap(~whereTo, scales = "fixed")

bar = ggplot(useTable[did.switch != 'NA' & (id %in% c(18,19,22,23,28,30,31,32,33,35,38,40,41,43,44,46,47,48,49)),list(mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo,JmenoOrientacnihoBodu)],aes(x=test.phase,y=mean,fill=c(did.switch)))
bar+geom_bar(stat='identity',position='dodge') + facet_grid(whereTo~JmenoOrientacnihoBodu, scales = "fixed")

bar = ggplot(useTable[did.switch != 'NA' & (!id %in% c(18,19,22,23,28,30,31,32,33,35,38,40,41,43,44,46,47,48,49)),list(mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo)],aes(x=test.phase,y=mean,fill=c(did.switch)))
bar+geom_bar(stat='identity',position='dodge') + facet_wrap(~whereTo, scales = "fixed")

bar = ggplot(useTable[did.switch != 'NA' & !(id %in% c(18,19,22,23,28,30,31,32,33,35,38,40,41,43,44,46,47,48,49)) & test.phase != 'F3',list(mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo,JmenoOrientacnihoBodu)],aes(x=test.phase,y=mean,fill=c(did.switch)))
bar+geom_bar(stat='identity',position='dodge') + facet_grid(whereTo~JmenoOrientacnihoBodu, scales = "fixed")

bar = ggplot(useTable[did.switch != 'NA' & !(id %in% c(18,19,22,23,28,30,31,32,33,35,38,40,41,43,44,46,47,48,49)) & test.phase != 'F3',list(mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo,JmenoOrientacnihoBodu)],aes(x=test.phase,y=mean,fill=c(did.switch)))
bar+geom_bar(stat='identity',position='dodge') + facet_grid(whereTo~JmenoOrientacnihoBodu, scales = "fixed")

bar = ggplot(useTable[did.switch != 'NA' & test.phase != 'F3' & same.letters.x==F, list(mean=mean(pokus)),by=list(did.switch,test.phase,whereTo,JmenoOrientacnihoBodu)],aes(x=test.phase,y=mean,fill=c(did.switch)))
bar+geom_bar(stat='identity',position='dodge') + facet_grid(whereTo~JmenoOrientacnihoBodu, scales = "fixed")

useTable[did.switch != 'NA' ,list(sd=sd(reactionTime),mean=mean(pokus), mean2=mean(reactionTime)),by=list(did.switch,test.phase,whereTo)]

newTable[id==52,table(correct.ans)]

bar = ggplot(useTable[did.switch != 'NA' & id==83,list(mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo)],aes(x=test.phase,y=mean,fill=c(did.switch)))
bar+geom_bar(stat='identity',position='dodge') + facet_wrap(~whereTo, scales = "fixed")
