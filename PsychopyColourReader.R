setwd("U:/Vyzkum/Diplomka/Data/Psychopy/Colour/Ver3/")
library(data.table)
library(ggplot2)

data.dir = "U:/Vyzkum/Diplomka/Data/Psychopy/Colour/Ver3/"

files = list.files(data.dir, full.names = T)

read_file <- function (file){
     if(any(grepl(".csv", file))){
          table = read.csv(file)
          prep_table = as.data.table(table[-c(1),c(1:3,10:11,14:18)])
          return (prep_table)
     }
          
}

colour_table = do.call("rbind",lapply(files, read_file))

setnames(colour_table,c("key_resp_4.keys","key_resp_4.rt","participant"),c("key","reactionTime","id"))
colour_table[,key:=as.character(key)]
colour_table[,key:=substring(key,3,nchar(key)-2)]
colour_table[,reactionTime:=as.character(reactionTime)]

colour_table[,reactionTime:=strsplit(substring(reactionTime,2,nchar(reactionTime)-1),",")]
colour_table[,reactionTime:=as.numeric(sapply(reactionTime,"[",1))]

setkey(colour_table,id)

colour_table[,mean(reactionTime,na.rm=T),by=list(id,session)]

sapply(colour_table,class)


dt[,mean(reactionTime,na.rm=T),by=letter]



