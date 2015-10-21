library(data.table)
library(binhf)

read_file <- function (file){
     if(any(grepl(".csv", file))){
          table = read.csv(file)
          prep_table = as.data.table(table[-c(1),c(1:3,10:11,14:18)])
          return (prep_table)
     }
          
}

#function to prepare the table
colour_prep_table <-function(dir,num){
    
     files = list.files(dir, full.names = T)
     
     colour_table = do.call("rbind",lapply(files, read_file))
     #renames to more comprehensive
     setnames(colour_table,c("key_resp_4.keys","key_resp_4.rt","participant"),c("key","reactionTime","id"))
     
     setkey(colour_table,session,id)
     colour_table[,letter.prev:=shift(as.character(letter),1),by=list(session,id)]
     colour_table[,colour.prev:=shift(as.character(colour),1),by=list(session,id)]
     
     colour_table[letter.prev=='',letter.prev:=NA]
     colour_table[colour.prev=='',colour.prev:=NA]
     #cleans up the key column
     colour_table[,key:=as.character(key)]
     colour_table[,key:=substring(key,3,nchar(key)-2)]
     colour_table[,reactionTime:=as.character(reactionTime)]
     
     #cleans up the reaction time column
     colour_table[,reactionTime:=strsplit(substring(reactionTime,2,nchar(reactionTime)-1),",")]
     colour_table[,reactionTime:=as.numeric(sapply(reactionTime,"[",1))]
     
     #removes the empty rows
     colour_table=colour_table[letter!='']
     #adds information about exp.version
     colour_table[,exp.version:=num]
     colour_table=cbind(colour_table,newid=paste(colour_table$id,".",colour_table$exp.version,sep="",collpase=""))
     return(colour_table)
}

colour_remove_long_reactions <-function(table,max.time){
     pre=nrow(table)
     returnTable=table[reactionTime<max.time]
     post=nrow(returnTable)
     print(paste(c(as.character(pre-post),"zaznamù smazáno"),sep=" ",collapse=" "))
     return(returnTable)
}
