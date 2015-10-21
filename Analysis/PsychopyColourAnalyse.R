setwd("U:/Vyzkum/Diplomka/DiplomkaGit")
library(ggplot2)
library(data.table)
library(ez)
library(xtable)

source("PsychopyColourReader.R")


#reads the data and does some basic cleaning
colour_table <- rbind(colour_prep_table("U:/Vyzkum/Diplomka/Data/Psychopy/Colour/Ver1",1),colour_prep_table("U:/Vyzkum/Diplomka/Data/Psychopy/Colour/Ver3",2))

num.trials=nrow(colour_table[complete.cases(colour_table)])
#deletes rows with reactionTImes larger than 1
colour_table <- colour_remove_long_reactions(colour_table,1)

num.deleted<-num.trials-nrow(colour_table[complete.cases(colour_table)])
setkey(colour_table,id,letter,colour)

colour_table[,mean(reactionTime,na.rm=T),by=list(id,session)]

sapply(colour_table,class)

ezTable=colour_table[complete.cases(colour_table)]

aovTable=ezTable[,newid:=factor(newid)]
aovTable[,colour:=factor(colour)]
aovTable[,RT:=mean(reactionTime),by=.(colour,newid)]

stats.colour=ezStats(
     data=ezTable,
     dv=reactionTime,
     wid=newid,
     within=.(colour),
     within_full=.(colour),
)
plot.colour=ezPlot(
     data=ezTable,
     dv=reactionTime,
     wid=newid,
     within=.(colour),
     within_full=.(colour),
     x=colour,
)

model.colour=ezANOVA(
     data=ezTable,
     dv=reactionTime,
     wid=newid,
     within=.(colour),
     within_full=.(colour),
     return_aov=T,
     type=2
)

model.letter=ezANOVA(
     data=ezTable,
     dv=reactionTime,
     wid=newid,
     within=.(letter),
     within_full=.(letter),
     return_aov=T
)
model.letter=ezANOVA(
     data=ezTable,
     dv=reactionTime,
     wid=newid,
     within=.(letter),
     within_full=.(letter),
)
stats.letter=ezStats(
     data=ezTable,
     dv=reactionTime,
     wid=newid,
     within=.(letter),
     within_full=.(letter),
)
plot.letter=ezPlot(
     data=ezTable,
     dv=reactionTime,
     wid=newid,
     within=.(letter),
     within_full=.(letter),
     x=letter
)
plot.letter.singular= ggplot(ezTable[,list(mean=mean(reactionTime)),by=list(newid,letter)],aes(y=mean,x=letter,group=newid,colour=newid)) 
plot.letter.singular= plot.letter.singular+ geom_point() + geom_line() +theme(legend.position="none")+ xlab("Letter") + ylab("Mean reaction time")
plot.colour.singular= ggplot(ezTable[,list(mean=mean(reactionTime)),by=list(newid,colour)],aes(y=mean,x=colour,group=newid,colour=newid)) 
plot.colour.singular= plot.colour.singular + guides(group=FALSE)+ geom_point() + geom_line() +theme(legend.position="none")+ xlab("Colour") + ylab("Mean reaction time")

checkIfClearColour<-function(myId){
     mod=summary(aov(reactionTime~colour,ezTable[newid==myId]))[[1]]
     p=as.numeric(mod$"Pr(>F)"[1])
     return(p)
}

checkEtaColour<-function(myId){
     mod=summary(aov(reactionTime~colour,ezTable[newid==myId]))[[1]]
     eta=mod$"Sum Sq"[1]/mod$"Sum Sq"[1]+mod$"Sum Sq"[2]
     return(eta)
}

subjects=unique(ezTable[,newid])
tabColour=cbind.data.frame("newid"=subjects,"p"=sapply(subjects,checkIfClearColour),"eta"=sapply(subjects,checkEtaColour))

different.colour=tabColour[tabColour$p<0.05,]

#good=as.character(tab[tab$p>0.05,"newid"])

#plot.colour.singular=ggplot(ezTable[newid %in% different,list(mean=mean(reactionTime)),by=list(newid,colour)],aes(y=mean,x=colour,group=newid,colour=newid)) + geom_point() + geom_line()
