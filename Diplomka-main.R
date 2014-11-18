setwd("U:/Vyzkum/Diplomka/DiplomkaGit")
library(ggplot2)
library(data.table)

data.dir = "U:/Vyzkum/Diplomka/data/Unity/Ver3All/"
files = list.files(data.dir, full.names = T, recursive=T)
source("Data-reader.R")

log_table = do.call("rbind",lapply(files, read_file, key="LOG"))
test_table= do.call("rbind",lapply(files, read_file, key="TEST"))

better_log_table(log_table)
source("Data-prep.R")

write.table(newTable,"newTableVer3.txt",sep=";",row.names=F,quote=F)

#write.table(newTable,"newTable.txt",sep=";",row.names=F,quote=F)
#n=fread("newTable.txt")

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
