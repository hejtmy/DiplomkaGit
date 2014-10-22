setwd("U:/Vyzkum/Diplomka/")
library(ggplot2)
library(data.table)

data.dir = "U:/Vyzkum/Diplomka/data/Unity/"
files = list.files(data.dir, full.names = T)

read_file <- function (file, key){

     #searches for the log part
     
     if(any(grepl(key, file))){
          name=gsub(data.dir,"",file)
          #removes the txt part
          name=substring(name,0,nchar(name)-4)
          #creates a list with some values
          basic_columns=strsplit(name,"_",fixed=T)[[1]]
          
          #splits the name of the file into variables and bindes them into a data.frame
          basic_table= rbind(basic_columns)
          #gives columns the names
          c_names=c("id","rozmisteni","test.phase","zapis","date.time","date")
          colnames(basic_table)=c_names
          
          #path to the file
          path = paste(c(data.dir,name),sep="",collapse="")
          
          prep_table=fread(file,sep=";")
          prep_table=cbind(prep_table,basic_table)
          
          return (prep_table)
          
     }
}

log_table = do.call("rbind",lapply(files, read_file, key="LOG"))
test_table= do.call("rbind",lapply(files, read_file, key="TEST"))

better_log_table <-function(table){
     
     columns_position = c("PoziceHrace","PoziceAI", "CilArena1Pozice","CilArena2Pozice","CilAI1Pozice","CilAI2Pozice")
   
     for (column in columns_position){
          #turns the Vector3 into lists of 3 values
          table[,(column):=strsplit(substring(get(column),2,nchar(get(column))-1),",")]
     }
     
     xyz <- c(".x",".y",".z")
     
     for (column in columns_position){
          #turns the Vector3 into lists of 3 values
          i=1
          for (letter in xyz){
               name<-paste(column,letter,sep="")
               #incredibly complicated
               #log_table[,(name):=matrix(unlist(get(column)),ncol=3,byrow=T)[,i]]
               table[,(name):=sapply(get(column),"[",i)]
               i<-i+1
          }
          table[,(column):=NULL]
     }
}
better_log_table(log_table)

atan2(as.numeric(tail(log_table[Faze==1],1)$cil3pozice.z),as.numeric(tail(log_table[Faze==1],1)$cil3pozice.x))

file = NULL

if(grep(key,file)){ 
     print ("hey")
}
i=1
for (column in columns_position){
     i<-i+1
     #expr <- parse(text = paste0(colum, ":=(a)"))
     #log_table[,(column):=substring(log_table[[column]],1,nchar(log_table[[column]])-1)]    
}
class(log_table[,Hracpozice[1]])
head(log_table[,sapply(Hracpozice, "[", 1)])
class(log_table[,get("Hracpozice")])
