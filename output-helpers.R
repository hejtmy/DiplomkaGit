apaP<-function(value){
     if(is.na(value)){
          return(NA)
     }
     if (value < 0.001){
          return("p \\textless .001 ")
     }
     if (value < 0.01){
          return("p \\textless .01 ")
     }
     if (value < 0.03){
          return("p \\textless .03 ")
     }
     if (value < 0.05){
          return("p \\textless .05 ")
     }
     if (value < 0.1){
          return(paste("p = ",gsub("^(-?)0.", "\\1.", round(value,digits=3)),sep=""))
     }
     if (value >0.1){
          return(paste("p = ",gsub("^(-?)0.", "\\1.", format(value,digits=3)),sep=""))
     }
}
apaCor<-function(value){
     return(gsub("^(-?)0.", "\\1.", format(value,digits=3)))
}
apaPtable<-function(value){
     if(is.na(value)){
          return(NA)
     }
     if (value < 0.001){
          return("\\textless .001 ")
     }
     if (value < 0.01){
          return("\\textless .01 ")
     }
     if (value < 0.03){
          return("\\textless .03 ")
     }
     if (value < 0.05){
          return("\\textless .05 ")
     }
     if (value < 0.1){
          return(paste(gsub("^(-?)0.", "\\1.", round(value,digits=3)),sep=""))
     }
     if (value >0.1){
          return(paste(gsub("^(-?)0.", "\\1.", format(value,digits=3)),sep=""))
     }
     
}

###helper functions for the report
reportAnova<-function(myList){
     mauch=myList$model.all$"Mauchly's Test for Sphericity"
     spher=myList$model.all$`Sphericity Corrections`
     
     if(is.null(mauch)){
          fstat=paste("F(",myList$model.all$ANOVA$DFn[2],", ",myList$model.all$ANOVA$DFd[2],")= ",round(myList$model.all$ANOVA$F[2],digits=2), ", ",apaP(myList$model.all$ANOVA$p[2]),sep="")
          return(fstat)
     }
     
     if(mauch$p>0.05){
          fstat=paste("F(",myList$model.all$ANOVA$DFn[2],", ",myList$model.all$ANOVA$DFd[2],")= ",round(myList$model.all$ANOVA$F[2],digits=2), ", ",apaP(myList$model.all$ANOVA$p[2]),sep="")
          return(fstat)
     }    
     
     if(mauch$p<0.05){
          if(spher$GGe>0.75){
               #using Hugh-Feldt correction
               mauchly=paste("(Mauchly's test indicated that assumption of sphericity was violated, " ,apaP(mauch$p),"; Greenhouse-Geisser ($\\epsilon$ = ", round(spher$GGe,digits=2),"), degrees of freedon were corrected using Huynh-Feldt estimates of sphericity)",sep="")
               fstat=paste("F(",round(spher$HFe*myList$model.all$ANOVA$DFn[2],digits=2),", ",round(spher$HFe*myList$model.all$ANOVA$DFd[2],digits=2),") = ",round(myList$model.all$ANOVA$F[2],digits=2), ", ",apaP(spher$"p[HF]"),sep="")
               return(paste(fstat, mauchly,sep=" "))
          }
          if(spher$GGe<0.75){
               #using Hugh-Feldt correction
               mauchly=paste("(Mauchly's test indicated that assumption of sphericity was violated, " ,apaP(mauch$p),"; degrees of freedom were corrected using Greenhouse-Geisser ($\\epsilon$ = ", round(spher$GGe,digits=2),") estimates of sphericity)",sep="")
               fstat=paste("F(",round(spher$GGe*myList$model.all$ANOVA$DFn[2],digits=2),", ",round(spher$GGe*myList$model.all$ANOVA$DFd[2],digits=2),") = ",round(myList$model.all$ANOVA$F[2],digits=2), ", ",apaP(spher$"p[GG]"),sep="")
               return(paste(fstat, mauchly, sep=" "))
          } 
     }
}
reportAnovaMod<-function(myList,num=2){
     fstat=paste("F(",myList$model.all$ANOVA$DFn[num],", ",myList$model.all$ANOVA$DFd[num],")= ",round(myList$model.all$ANOVA$F[num],digits=2), ", ",apaP(myList$model.all$ANOVA$p[num]),sep="")
     return(fstat)
}
reportAnovaCorMod<-function(myList,num=2){
     mauch=myList$model.all.rtCorr$"Mauchly's Test for Sphericity"
     spher=myList$model.all.rtCorr$`Sphericity Corrections`
     
     if(is.null(mauch)){
          fstat=paste("F(",myList$model.all.rtCorr$ANOVA$DFn[num],", ",myList$model.all.rtCorr$ANOVA$DFd[num],")= ",round(myList$model.all.rtCorr$ANOVA$F[num],digits=2), ", ",apaP(myList$model.all.rtCorr$ANOVA$p[num]),sep="")
          return(fstat)
     }
     
     if(mauch$p[num-2]>0.05){
          fstat=paste("F(",myList$model.all.rtCorr$ANOVA$DFn[num],", ",myList$model.all.rtCorr$ANOVA$DFd[num],")= ",round(myList$model.all.rtCorr$ANOVA$F[num],digits=2), ", ",apaP(myList$model.all.rtCorr$ANOVA$p[num]),sep="")
          return(fstat)
     }    
     
     if(mauch$p[num-2]<0.05){
          if(spher$GGe[num-2]>0.75){
               #using Hugh-Feldt correction
               mauchly=paste("(Mauchly's test indicated that assumption of sphericity was violated, " ,apaP(mauch$p[num-2]),"; Greenhouse-Geisser ($\\epsilon$ = ", round(spher$GGe[num-2],digits=2),"), degrees of freedon were corrected using Huynh-Feldt estimates of sphericity)",sep="")
               fstat=paste("F(",round(spher$HFe[num-2]*myList$model.all.rtCorr$ANOVA$DFn[num],digits=2),", ",round(spher$HFe[num-2]*myList$model.all.rtCorr$ANOVA$DFd[num],digits=2),") = ",round(myList$model.all.rtCorr$ANOVA$F[num],digits=2), ", ",apaP(spher$"p[HF]"[num-2]),sep="")
               return(paste(fstat, mauchly,sep=" "))
          }
          if(spher$GGe[num-2]<0.75){
               #using Hugh-Feldt correction
               mauchly=paste("(Mauchly's test indicated that assumption of sphericity was violated, " ,apaP(mauch$p[num-2]),"; degrees of freedom were corrected using Greenhouse-Geisser ($\\epsilon$ = ", round(spher$GGe[num-2],digits=2),") estimates of sphericity)",sep="")
               fstat=paste("F(",round(spher$GGe[num-2]*myList$model.all.rtCorr$ANOVA$DFn[num],digits=2),", ",round(spher$GGe[num-2]*myList$model.all.rtCorr$ANOVA$DFd[num],digits=2),") = ",round(myList$model.all.rtCorr$ANOVA$F[num],digits=2), ", ",apaP(spher$"p[GG]"[num-2]),sep="")
               return(paste(fstat, mauchly, sep=" "))
          } 
     }
}
reportAnovaCor<-function(myList){
     mauch=myList$model.all.rtCorr$"Mauchly's Test for Sphericity"
     spher=myList$model.all.rtCorr$`Sphericity Corrections`
     
     if(is.null(mauch)){
          #checks if t-test
          if (myList$model.all.rtCorr$ANOVA$DFn[2]==1){
          fstat=paste("t(",myList$model.all.rtCorr$ANOVA$DFd[2],")= ",round(myList$model.all.rtCorr$ANOVA$F[2],digits=2), ", ",apaP(myList$model.all.rtCorr$ANOVA$p[2]),sep="")     
          } else {    
          fstat=paste("F(",myList$model.all.rtCorr$ANOVA$DFn[2],", ",myList$model.all.rtCorr$ANOVA$DFd[2],")= ",round(myList$model.all.rtCorr$ANOVA$F[2],digits=2), ", ",apaP(myList$model.all.rtCorr$ANOVA$p[2]),sep="")
          }
          return(fstat)
     }
     
     if(mauch$p>0.05){
          fstat=paste("F(",myList$model.all.rtCorr$ANOVA$DFn[2],", ",myList$model.all.rtCorr$ANOVA$DFd[2],")= ",round(myList$model.all.rtCorr$ANOVA$F[2],digits=2), ", ",apaP(myList$model.all.rtCorr$ANOVA$p[2]),sep="")
          return(fstat)
     }    
     
     if(mauch$p<0.05){
          if(spher$GGe>0.75){
               #using Hugh-Feldt correction
               mauchly=paste("(Mauchly's test indicated that assumption of sphericity was violated, " ,apaP(mauch$p),"; Greenhouse-Geisser ($\\epsilon$ = ", round(spher$GGe,digits=2),"), degrees of freedon were corrected using Huynh-Feldt estimates of sphericity)",sep="")
               fstat=paste("F(",round(spher$HFe*myList$model.all.rtCorr$ANOVA$DFn[2],digits=2),", ",round(spher$HFe*myList$model.all.rtCorr$ANOVA$DFd[2],digits=2),") = ",round(myList$model.all.rtCorr$ANOVA$F[2],digits=2), ", ",apaP(spher$"p[HF]"),sep="")
               return(paste(fstat, mauchly,sep=" "))
          }
          if(spher$GGe<0.75){
               #using Hugh-Feldt correction
               mauchly=paste("(Mauchly's test indicated that assumption of sphericity was violated, " ,apaP(mauch$p),"; degrees of freedom were corrected using Greenhouse-Geisser ($\\epsilon$ = ", round(spher$GGe,digits=2),") estimates of sphericity)",sep="")
               fstat=paste("F(",round(spher$GGe*myList$model.all.rtCorr$ANOVA$DFn[2],digits=2),", ",round(spher$GGe*myList$model.all.rtCorr$ANOVA$DFd[2],digits=2),") = ",round(myList$model.all.rtCorr$ANOVA$F[2],digits=2), ", ",apaP(spher$"p[GG]"),sep="")
               return(paste(fstat, mauchly, sep=" "))
          } 
     }
}

reportRTDistanceCorrelation<-function(myList){
     return(paste("r(",length(myList$subset$reactionTime),") = ", apaCor(cor.test(myList$subset$reactionTime,myList$subset$distance1)$estimate),", ",apaP(cor.test(myList$subset$reactionTime,myList$subset$distance1)$p.value),sep=""))
}
