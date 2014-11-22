setwd("U:/Vyzkum/Diplomka/DiplomkaGit")
library(ggplot2)
library(data.table)

source("PsychopyColourReader.R")

data.dir = "U:/Vyzkum/Diplomka/Data/Psychopy/Colour/Ver1"

#reads the data and does some basic cleaning
colour_table <- colour_prep_table(data.dir)

#deletes rows with reactionTImes larger than 1
colour_table <- colour_remove_long_reactions(colour_table,1)

setkey(colour_table,id,letter,colour)

colour_table[,mean(reactionTime,na.rm=T),by=list(id,session)]

sapply(colour_table,class)

colour_table[,list(mean=mean(reactionTime,na.rm=T),sd=sd(reactionTime,na.rm=T)),by=list(colour,letter)]

#fits the model
fit<-aov(reactionTime~colour*letter+Error(id),colour_table)

fit2<-aov(reactionTime~colour*letter*colour.prev*letter.prev,colour_table[colour.prev!=""])

fit3<-aov(reactionTime~colour.prev,colour_table)

print(model.tables(fit,"means"),digits=5)

#simple anova
anova(fit)

#summary of the model
summary(fit)

#lets see where the significance occurs
tab=TukeyHSD(fit)$"colour:letter"
tab[tab[,"p adj"]<0.1,]

tab=TukeyHSD(fit2)$"colour.prev:letter.prev"
