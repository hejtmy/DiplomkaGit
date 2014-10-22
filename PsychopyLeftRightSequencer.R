createSequenceLeftRight <- function (num){
     set.seed(num)
     suffix="1600x999.jpg"
     possib=seq(0,355,5)
     seq=sample(possib)
     len = sample(c(20,25,30),72,replace=T)
     table = data.frame(angle=integer(length(seq)),image=character(length(seq)),answer=character(length(seq)),stringsAsFactors=F)
     line = 1
     for (i in seq){
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
