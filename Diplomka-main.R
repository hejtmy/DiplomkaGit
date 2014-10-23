setwd("U:/Vyzkum/Diplomka/")
library(ggplot2)
library(data.table)

data.dir = "U:/Vyzkum/Diplomka/data/Unity/"

source("Data-reader.R")

#log_table[,max(Faze),by=(test.phase)]

source("data-prep.R")

bar = ggplot(useTable[did.switch != 'NA',list(mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo)],aes(x=test.phase,y=mean,fill=c(did.switch)))
bar+geom_bar(stat='identity',position='dodge') + facet_wrap(~whereTo, scales = "free")

useTable[did.switch != 'NA',list(sd=sd(reactionTime)),by=list(did.switch,test.phase,whereTo)]

useTable[did.switch != 'NA',list(mean=mean(reactionTime)),by=list(did.switch,test.phase,whereTo)]

