read_file <- function (file, key, dir){
     #searches for the log part
     if(any(grepl(key, file))){
          name=gsub(dir,"",file)
          #removes the txt part
          name=substring(name,1,nchar(name)-4)
          
          #gets a folder name to paste
          #folder=strsplit(name,"/",fixed=T)[[1]][2]
          #folder.name=paste(c("/",folder,"/"),sep="",collapse="")
          #name=gsub(folder.name,"",name)
          
          #creates a list with some values
          basic_columns=c(strsplit(name,"_",fixed=T)[[1]])
          
          #splits the name of the file into variables and bindes them into a data.frame
          basic_table= rbind(basic_columns)
          #gives columns the names
          c_names=c("id","rozmisteni","test.phase","zapis","date.time","date")
          colnames(basic_table)=c_names
          
          #path to the file
          path = paste(c(dir,name),sep="",collapse="")
          
          prep_table=fread(file,sep=";")
          prep_table=cbind(prep_table,basic_table)
          
          return (prep_table)
          
     }
}

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

add_demographics<-function(table,filename){
     demograph<-fread(filename,colClasses=c("character","integer","integer","integer","time","integer","character"))
     demograph[,c("V1","maps ID"):=NULL]
     setnames(demograph,c("My ID","Version"),c("id","exp.version"))
     demograph[gender=='M',gender:='male']
     demograph[gender=='F',gender:='female']
     demograph[,id:=as.character(id)]
     setkey(demograph,id)
     setkey(table,id)
     return(merge(table,demograph))
}
#atan2(as.numeric(tail(log_table[Faze==1],1)$cil3pozice.z),as.numeric(tail(log_table[Faze==1],1)$cil3pozice.x))
