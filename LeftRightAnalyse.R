setwd("U:/Vyzkum/Diplomka/DiplomkaGit")
library(ggplot2)
library(data.table)
library(ez)
library(xtable)

source("PsychopyLeftRightReader.R")

letter_table <- rbind(letter_prep_table("U:/Vyzkum/Diplomka/Data/Psychopy/LeftRight/Ver1",1),letter_prep_table("U:/Vyzkum/Diplomka/Data/Psychopy/LeftRight/Ver3",2))

corTable=letter_remove_long_reactions(letter_table,3)
letter_table[,angle.dif:=abs(as.numeric(angle)-as.numeric(angle.prev))]
corTable=letter_table[correct==1]
cor(corTable$reactionTime,corTable$angle.dif,use = "complete.obs")
