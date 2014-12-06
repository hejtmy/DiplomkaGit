library(data.table)
library(binhf)

read_file <- function (file){
     if(any(grepl(".csv", file))){
          table = fread(file)
          return (table)
     }    
}

#function to prepare the table
letter_prep_table <-function(dir,num){
     
     files = list.files(dir, full.names = T)
     
     leftright_table = do.call("rbind",lapply(files, read_file))
     leftright_table = leftright_table[answer_image=="" & image!=""]
     #renames to more comprehensiv
     leftright_table=leftright_table[,c(2:4,21:28),with = FALSE]
     
     setnames(leftright_table,c("key_resp_2.keys","key_resp_2.rt","key_resp_2.corr","participant"),c("key","reactionTime","correct","id"))
     
     setkey(leftright_table,id)
     leftright_table[,angle.prev:=c("",angle[1:.N-1]),by=id]
     
     leftright_table=leftright_table[angle.prev=="",angle.prev:=NA]

     leftright_table[,exp.version:=num]
     leftright_table=cbind(leftright_table,newid=paste(leftright_table$id,".",leftright_table$exp.version,sep="",collpase=""))
     return(leftright_table)
}

letter_remove_long_reactions <-function(table,max.time){
     pre=nrow(table)
     returnTable=table[reactionTime<max.time]
     post=nrow(returnTable)
     print(paste(c(as.character(pre-post),"zaznamù smazáno"),sep=" ",collapse=" "))
     return(returnTable)
}