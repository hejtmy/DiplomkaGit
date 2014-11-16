library(data.table)
library(ggplot2)
setwd("U:/Vyzkum/Diplomka/Data/Psychopy/LeftRight/Ver3/")

data.dir = "U:/Vyzkum/Diplomka/Data/Psychopy/LeftRight/Ver3/"

data.dir = "C:/Lukas/Vyzkum/Diplomka/DiplomkaGit/dataPsy/"

files = list.files(data.dir, full.names = T)
read_file <- function (file){
     if(any(grepl(".csv", file))){
          mytable = read.csv(file)
          #deletes rows from the instruction part
          prep_table = as.data.table(mytable[nchar(as.character(mytable$answer_image))==0,c(2:4,21:29)])
          return (prep_table)
     }    
}

leftright_table= do.call("rbind",lapply(files, read_file))

setnames(leftright_table,c("key_resp_2.keys","key_resp_2.corr","key_resp_2.rt","participant"),c("klavesa","spravnaOdpoved","reactionTime","id"))


table(leftright_table[,spravnaOdpoved,by=id])

checkCorr <- function (key1,key2){
     
     if(as.character(key1)==as.character(key2)){
          return(1)
     } else {
          return (0)
     }
}

leftright_table[,key_resp_2.corr:=lapply(c(answer,key_resp_2.keys),checkCorr)]