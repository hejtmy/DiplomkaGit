createColourExp <- function(kodJ,kodB){
     set.seed(2014)
     sequence = createSequence(4,5)
     sequence = strsplit(sequence,"",fixed=T)[[1]]
     sequence = sapply(sequence,as.numeric)
     
     kod = createColourSetup(kodJ,kodB)
     table = data.frame(colour=character(length(sequence)),letter=character(length(sequence)), time=numeric(length(sequence)),stringsAsFactors=F)
     line = 1
     for (i in sequence){
          table$colour[line]<-kod[["barvy"]][i]
          table$letter[line]<-kod[["pismena"]][i]
          table$time[line] <- runif(1,1.5,3.5)
          line = line + 1
     }
     
     #creates file connection
     return(table)
}

createColourSetup <- function(kodJ,kodB){
     colours = c("Yellow","Red","Blue","Green")
     letters = c("A","B","C","D")
     
     kodJ = strsplit(gsub("kod_jmena=","",kodJ),"",fixed=T)[[1]]
     kodB = strsplit(gsub("kod_barvy=","",kodB),"",fixed=T)[[1]]
     
     barvy <- c()
     pismena <- c()
     for (i in kodB){
          barvy <- c(barvy,colours[as.integer(i)])
     }
     for (i in kodJ){
          pismena <- c(pismena,letters[as.integer(i)])
     }
     return(list("barvy"=barvy,"pismena"=pismena))
}