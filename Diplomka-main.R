setwd("U:/Vyzkum/Diplomka/DiplomkaGit")
library(ggplot2)
library(data.table)

data.dir = "U:/Vyzkum/Diplomka/data/Unity/Ver3Corr/"

source("Data-reader.R")

#log_table[,max(Faze),by=(test.phase)]

source("Data-prep.R")

t=data.frame(newTable[test.phase!="F3",correct.ans,by=list(id,test.phase)])

#checks the correct answers
library(scales)
corrplot=ggplot(t,aes(x=id,fill=correct.ans,y = (..count..)*4/sum(..count..)))
corrplot+geom_bar(stat='bin')+facet_wrap(~test.phase)+ scale_y_continuous(labels=percent)+ geom_hline(aes(yintercept = 0.80))

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

bar = ggplot(useTable[did.switch != 'NA' & test.phase != 'F3', list(mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo,JmenoOrientacnihoBodu)],aes(x=test.phase,y=mean,fill=c(did.switch)))
bar+geom_bar(stat='identity',position='dodge') + facet_grid(whereTo~JmenoOrientacnihoBodu, scales = "fixed")


useTable[did.switch != 'NA' & reactionTime,list(sd=sd(reactionTime),mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo)]

newTable[id==53,table(correct.ans)]

bar = ggplot(useTable[did.switch != 'NA' & (!id %in% c(18,19,22,23,28,30,31,32,33,35,38,40,41,43,44,46,47,48,49)),list(mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo)],aes(x=test.phase,y=mean,fill=c(did.switch)))
bar+geom_bar(stat='identity',position='dodge') + facet_wrap(~whereTo, scales = "fixed")