createColourExp <- function(kodJ,kodB){
     #sets seed for reproducibility
     #each proband has the same delays between letters
     #each combination of letters and coloures can be compared to ane another
     set.seed(2014)
     
     #loads sequencer script and randomises a sequence
     sequence = createSequence(4,5)
     #changes it into a vector
     sequence = strsplit(sequence,"",fixed=T)[[1]]
     #changes each char. object into a numeric object
     sequence = sapply(sequence,as.numeric)
     
     #uses data from the Unity ini file to create appropriate letter-colour combination
     kod = createColourSetup(kodJ,kodB)
     #prepares a table to insert the data into
     table = data.frame(colour=character(length(sequence)),letter=character(length(sequence)), time=numeric(length(sequence)),stringsAsFactors=F)
     #fills the table
     line = 1
     for (i in sequence){
          #assigns a correct colour
          table$colour[line]<-kod[["barvy"]][i]
          #asssigns a letter
          table$letter[line]<-kod[["pismena"]][i]
          #how long will the letter be displayed
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