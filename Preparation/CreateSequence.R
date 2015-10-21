createSequence <- function (units,iterations,begin="",samePair=F,bool=FALSE){
     
     #creates matrix with possible iterations
     matrix = matrix(data=iterations,nrow=units,ncol=units)
     
     #creates empty matrix to be compared with the matix state
     #If matrix == fullMatrix the script has nowhere to go and finnishes
     fullMatrix = matrix(data=0,nrow=units,ncol=units)
     
     #creates third empty matrix that fill us when possible units dissapear from matrix
     
     discardedMatrix = matrix(data=0,nrow=units,ncol=units)
     discardedMatrix2 = matrix(data=0,nrow=units,ncol=units)
     
     #fills in the matrices# 
     if (!samePair){
          for (n in 1:units){
               matrix[n,n]=0
          }
     }
     #if we want to incoporate beginning of the sequence
     if (nchar(begin)>0){
          sequence=begin
          for (i in 1:(nchar(begin)-1)){
               a<-as.integer(substr(begin,i,i))
               last<-as.integer(substr(begin,i+1,i+1))
               matrix[a,last] <- matrix [a,last]-1
          }
     #and if we don't
     } else {
          #creates the first unit in the sequence
          last = sample.int(units,1,replace=bool)
          
          #starts writing the sequence
          sequence = toString(last)
     }
     #now function starts creating the sequence untill it has nowhere to go
     while (!all(matrix==fullMatrix)){
          assigned = FALSE
          x=0
          # this loop randomly chooses next unit and if the itteration is possible it assigns it and skips forwards
          while (!assigned){
               possib = sample.int(units,1,replace=bool)
               if (matrix[last,possib]!=0){
                    # lowers the number of iterations
                    matrix[last,possib]=matrix[last,possib]-1
                    assigned = TRUE
               }
               x=x+1
               if (x>100){
                    return("Couldn't do it");    
               }
          }
          
          # if a row becomes empty we can't return to it anymore, as it wont be able to iterate to other unit
          # therefore we delete the possibility to return to it from any other unit
          if (all(matrix[last,]==fullMatrix[last,])){
               for (i in 1:units){
                    # but make note of discarded iterations to discardedMatrix
                    discardedMatrix[i,last]=matrix[i,last]
                    matrix[i,last]=0
               }
          }
          last = possib
          #adds to the sequence
          sequence=paste(c(sequence,toString(last)),sep="",collapse="")
     }
     
     discardedMatrix2 = discardedMatrix
     last2 = last
     sequence2 = sequence
     
     while(!all(discardedMatrix==fullMatrix)){
          assigned=FALSE
          if (all(discardedMatrix[last,]==fullMatrix[last,])){
               discardedMatrix = discardedMatrix2
               last = last2
               sequence=sequence2
          }
          while(!assigned){
               possib = sample.int(units,1,replace=bool)
               if(discardedMatrix[last,possib]!=0){
                    discardedMatrix[last,possib]=discardedMatrix[last,possib]-1
                    last = possib
                    sequence=paste(c(sequence,toString(last)),sep="",collapse="")
                    assigned=TRUE
               }     
          
          }

     }
     return(sequence)
}

checkSequence <- function (sequence,num){
     mat<-matrix(0,nrow=num,ncol=num,byrow=TRUE)
     for (i in 1:(nchar(sequence)-1)){
          a<-as.integer(substr(sequence,i,i))
          b<-as.integer(substr(sequence,i+1,i+1))
          mat[a,b] <- mat [a,b]+1
     }
     return(mat)
}

createLeftRightIteration <- function (sequence,units,iterations){
     
     mat=matrix(0,nrow=units,ncol=units)
     #matrices we will substract from
     left=matrix(iterations/2,nrow=units,ncol=units)
     right=matrix(iterations/2,nrow=units,ncol=units)
     #creates the first unit in the sequence
     last = sample.int(2,1,replace=T)-1
     
     #starts writing the sequence
     mySequence = toString(last)
     
     for (i in 1:(nchar(sequence)-1)){
          a<-as.integer(substr(sequence,i,i))
          b<-as.integer(substr(sequence,i+1,i+1))
          
          asgn=F;
          while (!asgn){
               last = sample.int(2,1,replace=T)-1
               if (last==0){
                    if (left[a,b]!=0){
                         left[a,b]=left[a,b]-1;
                         asgn=T;
                    }
               }
               if (last==1){
                    if (right[a,b]!=0){
                         right[a,b]=right[a,b]-1;
                         asgn=T;
                    }
               }
          }
          mySequence=paste(c(mySequence,toString(last)),sep="",collapse="")
     }
     return(mySequence)    
}

createTurnSequence = function (units,iterations,samePairX=F){
     seq2=createSequence(units,iterations,samePair=samePairX)
     seq1=createLeftRightIteration(seq2,units,iterations)
     mySequence=""
     for (i in 1:nchar(seq1)){
          mySequence=paste(c(mySequence,substr(seq1,i,i)),sep="",collapse="")
          mySequence=paste(c(mySequence,substr(seq2,i,i)),sep="",collapse="")
     }
     return(mySequence)
}