setwd("U:/Dimplomka/PsychoPyExp/data/")
library(data.table)

table = read.csv("999_Experiment_2014_X_13_2053.csv")
dt = as.data.table(table[-c(1:2),c(1:7,13:17)])
dt[,reaction_time:=strsplit(substring(as.character(mouse_3.time),2,nchar(as.character(mouse_3.time))-1),",")]
dt[,reaction_time:=as.numeric(sapply(reaction_time,"[",1))]
sapply(dt,class)


dt[,mean(reaction_time,na.rm=T),by=letter]
