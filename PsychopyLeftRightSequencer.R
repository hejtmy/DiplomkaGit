createSequenceLeftRight <- function (num){
     set.seed(num)
     suffix="1600x999.jpg"
     possib=seq(0,355,5)
     seqen=sample(possib)
     len = sample(c(20,25,30),72,replace=T)
     table = data.frame(angle=integer(length(seqen)),image=character(length(seqen)),answer=character(length(seqen)),stringsAsFactors=F)
     line = 1
     for (i in seqen){
          if (i<=180){
               namePrep <- as.character(i)
          } else { 
               namePrep <- paste(c("inv_",as.character(360-i)),sep="",collapse="")
          }
          
          if (i <=230 && i >=55){
               answ = "left"
          } else {
               answ = "right"
          }
          
          name <- paste(c("images\\",namePrep,"_",as.character(len[line]),"_",suffix),sep="",collapse="")
          table$angle[line]<-i
          table$image[line]<-name
          table$answer[line]<-answ
          line = line + 1
     }
     return (table)
     
}

createSequencePureLeftRight <-function(num){
     set.seed(num)
     suffix="1600x999.jpg"
     possib=seq(0,355,5)
     seqen=sample(possib)
     remove = c(90,270)
     seqen=seqen[! seqen %in% remove]
     len = sample(c(20,25,30),72,replace=T)
     table = data.frame(angle=integer(length(seqen)),image=character(length(seqen)),answer=character(length(seqen)),stringsAsFactors=F)
     line = 1
     for (i in seqen){
          if (i<=180){
               namePrep <- as.character(i)
          } else { 
               namePrep <- paste(c("inv_",as.character(360-i)),sep="",collapse="")
          }
          
          if (i >=95 && i <=265){
               answ = "left"
          } else {
               answ = "right"
          } 
          
          name <- paste(c("images\\",namePrep,"_",as.character(len[line]),"_",suffix),sep="",collapse="")
          table$angle[line]<-i
          table$image[line]<-name
          table$answer[line]<-answ
          line = line + 1
     }
     return (table)
}

createSequenceFrontBack <-function(num){
     set.seed(num)
     suffix="1600x999.jpg"
     possib=seq(0,355,5)
     seqen=sample(possib)
     remove = c(0,180)
     seqen=seqen[! seqen %in% remove]
     len = sample(c(20,25,30),72,replace=T)
     table = data.frame(angle=integer(length(seqen)),image=character(length(seqen)),answer=character(length(seqen)),stringsAsFactors=F)
     line = 1
     for (i in seqen){
          if (i<=180){
               namePrep <- as.character(i)
          } else { 
               namePrep <- paste(c("inv_",as.character(360-i)),sep="",collapse="")
          }
          
          if (i >=5 && i <=175){
               answ = "right"
          } else {
               answ = "left"
          } 
          
          name <- paste(c("images\\",namePrep,"_",as.character(len[line]),"_",suffix),sep="",collapse="")
          table$angle[line]<-i
          table$image[line]<-name
          table$answer[line]<-answ
          line = line + 1
     }
     return (table)
}