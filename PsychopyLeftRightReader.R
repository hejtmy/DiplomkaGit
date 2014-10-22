setwd("U:/Vyzkum/Diplomka")
library(data.table)
table=read.csv("1_Experiment_2014_X_15_2316.csv")
dt = as.data.table(table[c(1:3,16:26))
dt[,key_resp_2.corr:=lapply(c(answer,key_resp_2.keys),checkCorr)]

checkCorr <- function (key1,key2){
     
     if(as.character(key1)==as.character(key2)){
          return(1)
     } else {
          return (0)
     }
}